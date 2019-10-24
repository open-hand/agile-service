import React, { Component } from 'react';
import PropTypes from 'prop-types';
import { observer } from 'mobx-react';
import { DropTarget } from 'react-dnd';
import Column from '../Column';
import StoryCard from './StoryCard';
import CreateStory from './CreateStory';
import StoryMapStore from '../../../../../../stores/project/StoryMap/StoryMapStore';
import './StoryColumn.less';

@observer
class StoryColumn extends Component {
  handleCreateStory = (newStory) => {
    StoryMapStore.afterCreateStory(newStory);
  }


  render() {
    // console.log('render');
    const {
      storys, width, epic, feature, version, connectDropTarget, isOver, rowIndex,
    } = this.props;
    // 只有未规划和规划中的可以创建
    const canCreate = version ? (!version.statusCode || version.statusCode === 'version_planning') : true;
    return (
      <Column
        width={width}
        saveRef={connectDropTarget}
        style={{ background: isOver ? 'rgb(240,240,240)' : 'white', position: 'relative' }}
      >
        <div style={{ display: 'flex', flexWrap: 'wrap' }}>
          {storys && storys.map((story, index) => <StoryCard index={index} rowIndex={rowIndex} story={story} version={version} />)}
          {!StoryMapStore.isFullScreen && canCreate && <CreateStory onCreate={this.handleCreateStory} epic={epic} feature={feature} version={version} />}
        </div>
      </Column>
    );
  }
}

StoryColumn.propTypes = {

};

export default DropTarget(
  'story',
  {
    drop: props => ({ epic: props.epic, feature: props.feature, version: props.version }),
  },
  (connect, monitor) => ({
    connectDropTarget: connect.dropTarget(),
    isOver: monitor.isOver(),
    // canDrop: monitor.canDrop(), //去掉可以优化性能
  }),
)(StoryColumn);
