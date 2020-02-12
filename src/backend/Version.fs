namespace GasDay.WebPlatform

open System.Diagnostics.CodeAnalysis

[<ExcludeFromCodeCoverage>]
type Version =
  {
    Version: string
  }

  static member Default = 
    {
      Version = "1.8.91.4"
    }