/*eslint-disable */
import React from 'react';
import _ from 'lodash';
import PropTypes from 'prop-types';
import Child from './components/Child';
import './react-minimap.less';
import ListenSize from '../ListenSize';
import StoryMapStore from '@/stores/project/StoryMap/StoryMapStore';

const buffer = 300;
export class Minimap extends React.Component {
  // static propTypes = {
  //   className: PropTypes.string,
  //   selector: PropTypes.string.isRequired,
  //   width: PropTypes.number /** in pixel */,
  //   height: PropTypes.number /** in pixel */,
  //   keepAspectRatio: PropTypes.bool,
  //   childComponent: PropTypes.any,
  //   onMountCenterOnX: PropTypes.bool,
  //   onMountCenterOnY: PropTypes.bool,
  // }

  // static defaultProps = {
  //   className: '',
  //   width: 200,
  //   height: 200,
  //   keepAspectRatio: false,
  //   childComponent: Child,
  //   onMountCenterOnX: false,
  //   onMountCenterOnY: false,
  // }

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

  handleScroll = () => {
    const {scrollEle} = this.state;
    const {
      pageSize, page, loading, totalPage, tableWidth,
    } = StoryMapStore;
    const enabledScrollWidth = (tableWidth || 0) - (scrollEle.clientWidth || 0) - (scrollEle.scrollLeft || 0);
    if (enabledScrollWidth > 0 && enabledScrollWidth <= buffer && !loading) {
      if (page * pageSize <= StoryMapStore.getEpicList.length && page < totalPage) {
        StoryMapStore.setPage(page + 1);
        StoryMapStore.getStoryMap();
      }
    }
  }
  render() {
    return (
      <div className={`minimap-container ${this.props.className}`}>
        <div
          className="minimap-container-scroll"
          ref={(container) => {
            this.source = container;
          }}
          onScroll={this.handleScroll}
        >
          {this.props.children}
        </div>
        <ListenSize scrollRef={this.source} />
      </div>
    );
  }
}

export default Minimap;
