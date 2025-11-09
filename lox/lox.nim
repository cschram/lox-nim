import scanner

when isMainModule:
  let source = """
  print "Hello, World!";
  """
  let result = scan(source)
  for token in result.tokens:
    echo $token
  for error in result.errors:
    echo $error
