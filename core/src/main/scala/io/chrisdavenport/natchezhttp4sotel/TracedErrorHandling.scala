package io.chrisdavenport.natchezhttp4sotel

import cats._
import cats.data.Kleisli
import cats.data.OptionT
import cats.effect.MonadCancelThrow
import cats.syntax.all._
import org.http4s._
import org.http4s.headers._
import org.typelevel.ci._
import org.typelevel.log4cats.LoggerFactory
import natchez.Trace

object ErrorHandling {
  def apply[F[_]: LoggerFactory: Trace, G[_]](
      k: Kleisli[F, Request[G], Response[G]]
  )(implicit F: MonadThrow[F]): Kleisli[F, Request[G], Response[G]] =
    Kleisli { req =>
      val pf: PartialFunction[Throwable, F[Response[G]]] =
        org.http4s.server.inDefaultServiceErrorHandler[F, G].apply(req)
      k.run(req).handleErrorWith { e =>

        pf.lift(e) match {
          case Some(resp) =>
            Trace[F].put(OTHttpTags.Errors.error(e):_*) *>
              resp
          case None => F.raiseError(e)
        }
      }
    }

  def httpRoutes[F[_]: MonadCancelThrow: LoggerFactory: Trace](httpRoutes: HttpRoutes[F]): HttpRoutes[F] = {
    implicit val factory: LoggerFactory[OptionT[F, *]] = LoggerFactory[F].mapK(OptionT.liftK)
    apply(httpRoutes)
  }

  def httpApp[F[_]: MonadThrow: LoggerFactory: Trace](httpApp: HttpApp[F]): HttpApp[F] =
    apply(httpApp)

  object Custom {
    def recoverWith[F[_]: MonadThrow: Trace, G[_], A](
        http: Kleisli[F, A, Response[G]]
    )(pf: PartialFunction[Throwable, F[Response[G]]]): Kleisli[F, A, Response[G]] =
      Kleisli { (a: A) =>
        http.run(a).handleErrorWith { e =>
          pf.lift(e) match {
            case Some(resp) =>
              Trace[F].put(OTHttpTags.Errors.error(e):_*) *>
                resp
            case None => ApplicativeThrow[F].raiseError(e)
          }
        }
      }
    
  }

  object Recover {

    def total[F[_]: MonadThrow: Trace, G[_], A](
        http: Kleisli[F, Request[G], Response[G]]
    ): Kleisli[F, Request[G], Response[G]] =
      Kleisli { (a: Request[G]) =>
        http.run(a).handleErrorWith(totalRecover(a.httpVersion))
      }

    def messageFailure[F[_]: MonadThrow: Trace, G[_], A](
        http: Kleisli[F, Request[G], Response[G]]
    ): Kleisli[F, Request[G], Response[G]] =
      Kleisli { (a: Request[G]) =>
        http.run(a).recoverWith(messageFailureRecover(a.httpVersion))
      }

    def messageFailureRecover[F[_]: MonadThrow: Trace, G[_]](
        httpVersion: HttpVersion
    ): PartialFunction[Throwable, F[Response[G]]] = { case m: MessageFailure =>

      Trace[F].put(OTHttpTags.Errors.error(m):_*).as(
        m.toHttpResponse[G](httpVersion)
      )
    }

    def totalRecover[F[_]: Trace: MonadThrow, G[_]](
        httpVersion: HttpVersion
    ): Throwable => F[Response[G]] = {
      case m: MessageFailure => Trace[F].put(OTHttpTags.Errors.error(m):_*).as(
        m.toHttpResponse[G](httpVersion)
      )
      case t =>
        Trace[F].put(OTHttpTags.Errors.error(t):_*).as(
          Response(
            Status.InternalServerError,
            httpVersion,
            Headers(
              Connection(ci"close"),
              `Content-Length`.zero,
            ),
          )
        )
    }

  }
}