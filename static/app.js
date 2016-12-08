const counter = (state = 0, action) => {
  switch (action.type) {
  case 'INCREMENT':
    return state + 1;
  default:
    return state;
  }
}

const Counter = ({value, onIncrement}) => (
    <div>
    <h1>{value}</h1>
    <button onClick={onIncrement}>+</button>
    </div>
);

const mapStateToProps = (state) => {
  return {value: state};
}

const mapDispatchToProps = (dispatch) => {
  return {
    onIncrement: () => {
      dispatch({
        type: 'INCREMENT'
      })
    }
  };
};

const CounterContainer = ReactRedux.connect(mapStateToProps, mapDispatchToProps)(Counter);
const store = Redux.createStore(counter);

ReactDOM.render(
    <CounterContainer store={store} />,
  document.getElementById('root')
);
