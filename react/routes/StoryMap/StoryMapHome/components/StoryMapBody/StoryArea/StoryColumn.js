import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { DropTarget } from 'react-dnd';
import { CardHeight } from '../../../Constants';
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
    const {
      storys, width, epic, feature, version, sprint, connectDropTarget, isOver, rowIndex,
    } = this.props;
    const { issueId: epicId } = epic;
    const { epicInViewportMap } = StoryMapStore;
    // 只有未规划、版本规划中、冲刺未完成的可以创建、删除、拖拽
    let canBeOperated = true;
    let id;
    if (version) {
      canBeOperated = !version.statusCode || version.statusCode === 'version_planning';
      id = version.versionId;
    }
    if (sprint) {
      canBeOperated = !sprint.statusCode || sprint.statusCode !== 'closed';
      id = sprint.sprintId;
    }

    return (
      <Column
        width={width}
        saveRef={connectDropTarget}
        style={{ background: isOver ? 'rgb(240,240,240)' : 'white', position: 'relative', minHeight: storys ? (storys.length + 1) * CardHeight : undefined }}
      >
        {
          (!!epicInViewportMap.get(epicId)) && (
            <div style={{ display: 'flex', flexWrap: 'wrap' }}>
              {storys && storys.map((story, index) => <StoryCard index={index} rowIndex={rowIndex} story={story} sprint={sprint} version={version} canBeOperated={canBeOperated} />)}
              {!StoryMapStore.isFullScreen && canBeOperated && <CreateStory onCreate={this.handleCreateStory} epic={epic} feature={feature} sprint={sprint} version={version} />}
            </div>
          )
        }
      </Column>
    );
  }
}

StoryColumn.propTypes = {

};

export default DropTarget(
  'story',
  {
    drop: (props) => ({
      epic: props.epic, feature: props.feature, version: props.version, sprint: props.sprint,
    }),
    canDrop: (props, monitor) => { // props: target, monitor: source
      const item = monitor.getItem();
      if (StoryMapStore.swimLine === 'version') {
        const targetVersion = props.version;
        const sourceVersion = item.version;

        if (targetVersion) { // 版本泳道
          const { versionId: targetVersionId, statusCode: targetVersionStatusCode } = targetVersion;
          const { versionId: sourceVersionId, statusCode: sourceVersionStatusCode } = sourceVersion || {};
          if (((targetVersionId === 'none' || targetVersionStatusCode !== 'released') && sourceVersionStatusCode !== 'released') || sourceVersionId === targetVersionId) { // 移入冲刺时不能是从已完成冲刺移出的，或者在自己冲刺内更改史诗或特性
            return true;
          }
          return false;
        }
      } else if (StoryMapStore.swimLine === 'sprint') {
        const targetSprint = props.sprint;
        const sourceSprint = item.sprint;

        if (targetSprint) { // 冲刺泳道
          const { sprintId: targetSprintId, statusCode: targetSprintStatusCode } = targetSprint;
          const { sprintId: sourceSprintId, statusCode: sourceSprintStatusCode } = sourceSprint || {};
          if (((targetSprintId === 'none' || targetSprintStatusCode !== 'closed') && sourceSprintStatusCode !== 'closed') || sourceSprintId === targetSprintId) { // 移入冲刺时不能是从已完成冲刺移出的，或者在自己冲刺内更改史诗或特性
            return true;
          }
          return false;
        }
      }
      return true;
    },
  },
  (connect, monitor) => ({
    connectDropTarget: connect.dropTarget(),
    isOver: monitor.isOver(),
    canDrop: monitor.canDrop(),
  }),
)(StoryColumn);
