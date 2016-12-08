const graph = (state=[], action) => {
  switch (action.type) {
  case 'CREATE':
    return state.concat([{
      name: "tbc" + state.length,
      x: action.location.x,
      y: action.location.y
    }])
  default:
    return state;
  }
}

class Circle extends React.Component {
  render() {
    return (
        <circle cx={this.props.x} cy={this.props.y} r={10} fill="red" />
    )
  }
}

const Graph = ({value, onCreate}) => (
    <svg onClick={onCreate} style={{width: "100%", height: "100%"}}>
    {value.map(node => <Circle key={node.name} x={node.x} y={node.y}>{node.name}</Circle>)}
    </svg>
);

const mapStateToProps = (state) => {
  return {value: state};
}

const mapDispatchToProps = (dispatch) => {
  return {
    onCreate: (event) => {
      dispatch({
        type: 'CREATE',
        location: {x: event.clientX, y: event.clientY}
      })
    }
  };
};

const GraphContainer = ReactRedux.connect(mapStateToProps, mapDispatchToProps)(Graph);
const store = Redux.createStore(graph, []);

ReactDOM.render(
    <GraphContainer store={store} />,
  document.getElementById('root')
);
