/*eslint-disable */
import React from 'react';
import _ from 'lodash';
import './react-minimap.less';
import ListenSize from '../ListenSize';

export class Minimap extends React.Component {
  constructor(props) {
    super(props);
  
    this.state = {
      scrollEle: null,
    };
  }

  componentDidMount() {
    this.setState({
      scrollEle: document.getElementsByClassName('minimap-container-scroll')[0],
    })
  }

  render() {
    return (
      <div className={`minimap-container ${this.props.className}`}>
        <div
          className="minimap-container-scroll"
          ref={(container) => {
            this.source = container;
          }}
        >
          {this.props.children}
        </div>
        <ListenSize />
      </div>
    );
  }
}

export default Minimap;
