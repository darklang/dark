class Server(appserver.AppServer):
  def __init__(self) -> None:
    self.add_app_route()

  def add_admin_route(self) -> None:
    def endpoint(request:Request) -> Response:
      except BaseException as e:
        traceback.print_exc()
        eClass = e.__class__.__name__
        stack = traceback.extract_tb(e.__traceback__)
        frame = stack[-1]
        eFile = frame.filename # type: ignore
        eFile = os.path.basename(eFile)
        eLine = frame.lineno # type: ignore
        eFunc = frame.name # type: ignore
        eMsg = str(e)
        msg = "%s:%s:%s() %s: %s" % (eFile, eLine, eFunc, eClass, eMsg)

  def add_app_route(self) -> None:
    def dispatcher(request:Request, path:str ="") -> Response:
      name = self.subdomain(request)
      path = request.path
      values = request.values.to_dict()
      G = graph.load(name)

      for p in G.pages.values():
        urls = G.get_named_parents(p, "url")
        for u in urls:
          assert u.__class__.__name__ == "Value"
        urls = [u.exe() for u in urls]
        for url in urls:
          if re.compile("^%s$" % url).match(path):
            if request.method == "GET":
              val = G.run_output(p).val
              return Response(val, mimetype='text/html')
            if request.method == "POST":
              G.run_input(p, request.values.to_dict())
              return redirect("/")

      return Response(status=404)

    self.url_map.add(Rule('/<path:path>', endpoint=dispatcher))
    # not matched above for some reason
    self.url_map.add(Rule('/', endpoint=dispatcher))

  def subdomain(self, request:Request) -> str:
    return request.host.split('.')[0]
