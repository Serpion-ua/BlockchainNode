package blockchain

object Config {
  case class RestServerConfig(host: String, port: Int)

  case class GenesisConfig(path: String)

  case class CryptoConfig(nodeKey: String)

  case class AppConfig(rest: RestServerConfig, genesis: GenesisConfig, crypto: CryptoConfig)
}
