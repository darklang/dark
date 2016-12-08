const graph = (state = 0, action) => {
  switch (action.type) {
  case 'CREATE':
    return state + 1;
  default:
    return state;
  }
}

const Graph = ({value, onCreate}) => (
    <div onClick={onCreate} style={{height: "100%"}}>
    <h1>{value}</h1>
    <button onClick={onCreate}>+</button>
    </div>
);

const mapStateToProps = (state) => {
  return {value: state};
}

const mapDispatchToProps = (dispatch) => {
  return {
    onCreate: () => {
      dispatch({
        type: 'CREATE'
      })
    }
  };
};

const GraphContainer = ReactRedux.connect(mapStateToProps, mapDispatchToProps)(Graph);
const store = Redux.createStore(graph);

ReactDOM.render(
    <GraphContainer store={store} />,
  document.getElementById('root')
);
