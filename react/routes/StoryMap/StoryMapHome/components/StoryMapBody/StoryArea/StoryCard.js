import React, { Component } from 'react';
import PropTypes from 'prop-types';
import { Icon, Tooltip, Modal } from 'choerodon-ui/pro';

import { DragSource } from 'react-dnd';
import { find } from 'lodash';
import { observer } from 'mobx-react';
import StatusTag from '@/components/StatusTag';
import { storyMapApi } from '@/api';

import { TypeTag } from '@/components';
import AutoScroll from '../../../../../../common/AutoScroll';
import Card from '../Card';
import './StoryCard.less';
import StoryMapStore from '../../../../../../stores/project/StoryMap/StoryMapStore';

@observer
class StoryCard extends Component {
  componentDidMount() {
    this.AutoScroll = new AutoScroll({
      scrollElement: document.getElementsByClassName('minimap-container-scroll')[0],
      pos: {
        left: 200,
        top: 150,
        bottom: 150,
        right: 150,
      },
      type: 'drag',
    });
  }

  setZIndex = () => {
    this.container.style.zIndex = 9999;
  }

  resetZIndex = () => {
    this.container.style.zIndex = 'unset';
  }

  saveRef = (ref) => {
    const { connectDragSource } = this.props;
    connectDragSource(ref);
    this.container = ref;
  }

  handleMouseDown = (e) => {
    this.AutoScroll.prepare(e);
  }

  handlRemoveStory = (e) => {
    const { story, version, sprint } = this.props;
    e.stopPropagation();
    Modal.open({
      title: '提示',
      children: (
        <div>
          {`确认要移除故事“${story?.summary}”？`}
        </div>
      ),
      onOk: async () => {
        const { issueId, storyMapVersionDTOList, storyMapSprintList } = story;
        const { swimLine } = StoryMapStore;
        // 未规划或无泳道
        if (swimLine === 'none' || (storyMapVersionDTOList.length === 0 && storyMapSprintList.length === 0)) {
          const storyMapDragVO = {
            // 工作项id列表，移动到版本，配合versionId使用
            // versionIssueIds: [],
            epicId: 0, // 要关联的史诗id
            epicIssueIds: [issueId],
            featureId: 0, // 要关联的特性id
            // 工作项id列表，移动到特性，配合featureId使用
            featureIssueIds: [issueId],
          };
          await storyMapApi.move(storyMapDragVO);
          StoryMapStore.removeStoryFromStoryMap(story);
          StoryMapStore.loadIssueList();
        } else if (swimLine === 'version') {
          const storyMapDragVO = {
            versionId: 0,
            versionIssueRelVOList: [],
          };
          if (storyMapVersionDTOList.length > 0) {
            const removeVersion = find(storyMapVersionDTOList, { versionId: version.versionId });
            storyMapDragVO.versionIssueRelVOList = [{ ...removeVersion, issueId }];
          }
          await storyMapApi.move(storyMapDragVO);
          StoryMapStore.removeStoryFromStoryMap(story, version.versionId);
          StoryMapStore.loadIssueList();
        } else if (swimLine === 'sprint') {
          const storyMapDragVO = {
            sprintId: 0,
            sprintIssueIds: [issueId],
          };
          await storyMapApi.move(storyMapDragVO);
          StoryMapStore.removeStoryFromStoryMap(story, sprint.sprintId);
          StoryMapStore.loadIssueList();
        }
        return true;
      },
      onCancel: () => true,
    });
  }

  handleClick = () => {
    const { story } = this.props;
    StoryMapStore.setClickIssue(story);
  }

  render() {
    const {
      story, index, rowIndex, canBeOperated, isDragging,
    } = this.props;
    const {
      issueId, summary, statusVO = {},
    } = story;
    const { selectedIssueMap } = StoryMapStore;
    return (
      <Card
        className={`c7nagile-StoryMap-StoryCard ${isDragging ? 'c7nagile-StoryMap-StoryCard-dragging' : ''} ${index === 0 && rowIndex === 0 ? 'minimapCard' : ''} ${statusVO && statusVO.completed ? 'completedCard' : undefined} ${selectedIssueMap.has(issueId) ? 'selected' : ''}`}
        saveRef={this.saveRef}
        onClick={this.handleClick}
        onMouseDown={this.handleMouseDown}
      >
        {
          canBeOperated && (
            <Icon type="close" className="c7nagile-StoryMap-StoryCard-delete" onClick={this.handlRemoveStory} />
          )
        }
        <div className="summary">
          <Tooltip title={summary}>
            {summary}
          </Tooltip>
        </div>
        <div className="bottom">
          <div className="status">
            <StatusTag
              data={statusVO || {}}
            />
          </div>
        </div>
      </Card>
    );
  }
}

StoryCard.propTypes = {

};

export default DragSource(
  'story',
  {
    beginDrag: (props, monitor, component) => {
      if (component && component.resetZIndex) {
        component.setZIndex();
        setTimeout(() => {
          component.resetZIndex();
        });
      }

      return { story: props.story, version: props.version, sprint: props.sprint };
    },
    endDrag(props, monitor) { // props: target, monitor: source
      const item = monitor.getItem();
      const dropResult = monitor.getDropResult();

      if (!dropResult) {
        return;
      }
      const { story, version: sourceVersion, sprint: sourceSprint } = item;
      const {
        issueId, epicId, storyMapVersionDTOList, storyMapSprintList,
      } = story;
      const featureId = story.featureId || 'none';
      const {
        epic: { issueId: targetEpicId }, feature: { issueId: targetFeatureId }, version, sprint,
      } = dropResult;
      const { versionId: targetVersionId } = version || {};
      const { sprintId: targetSprintId } = sprint || {};
      const storyMapDragVO = {}; // {
      //   versionIssueIds: [],
      //   versionId: 0, // 要关联的版本id
      //   epicId: 0, // 要关联的史诗id
      //   versionIssueRelVOList: [],
      //   // 工作项id列表，移动到史诗，配合epicId使用
      //   epicIssueIds: [],
      //   featureId: 0, // 要关联的特性id
      //   // 工作项id列表，移动到特性，配合featureId使用
      //   featureIssueIds: [],
      //   sprintIssueIds: [],
      //   sprintId: 0, // 要关联的版本id
      // }
      // 史诗，特性，版本，冲刺都不变时
      if (epicId === targetEpicId && featureId === targetFeatureId) {
        if (StoryMapStore.swimLine === 'version') {
          if (find(storyMapVersionDTOList, { versionId: targetVersionId }) || (storyMapVersionDTOList.length === 0 && targetVersionId === 'none')) {
            return;
          }
        } else if (StoryMapStore.swimLine === 'sprint') {
          if (find(storyMapSprintList, { sprintId: targetSprintId }) || (storyMapSprintList.length === 0 && targetSprintId === 'none')) {
            return;
          }
        } else if (StoryMapStore.swimLine === 'none') {
          return;
        }
      }
      if (epicId !== targetEpicId) {
        storyMapDragVO.epicId = targetEpicId;
        storyMapDragVO.epicIssueIds = [issueId];
      }
      if (featureId !== targetFeatureId) {
        // 移动到直接关联史诗的列，将史诗传过去，否则会将史诗清掉
        if (targetFeatureId === 'none') {
          storyMapDragVO.epicId = targetEpicId;
          storyMapDragVO.epicIssueIds = [issueId];
          storyMapDragVO.featureId = 0;
          storyMapDragVO.featureIssueIds = [issueId];
        } else {
          storyMapDragVO.featureId = targetFeatureId;
          storyMapDragVO.featureIssueIds = [issueId];
        }
      }
      // 对版本进行处理
      if (StoryMapStore.swimLine === 'version') {
        // 在不同的版本移动
        if (sourceVersion.versionId !== targetVersionId) {
          // 如果原先有版本，就移除离开的版本
          if (storyMapVersionDTOList.length > 0) {
            const removeVersion = find(storyMapVersionDTOList, { versionId: sourceVersion.versionId });
            if (removeVersion) {
              storyMapDragVO.versionIssueRelVOList = [{ ...removeVersion, issueId }];
            }
          }
        }

        if (!find(storyMapVersionDTOList, { versionId: targetVersionId })) {
          storyMapDragVO.versionIssueIds = [issueId];
          storyMapDragVO.versionId = targetVersionId === 'none' ? 0 : targetVersionId;
        }
      }

      // 对冲刺进行处理
      if (StoryMapStore.swimLine === 'sprint') {
        if (!find(storyMapSprintList, { sprintId: targetSprintId })) {
          storyMapDragVO.sprintIssueIds = [issueId];
          storyMapDragVO.sprintId = targetSprintId === 'none' ? 0 : targetSprintId;
        }
      }

      storyMapApi.move(storyMapDragVO).then(() => {
        StoryMapStore.setClickIssue(null);
        // StoryMapStore.removeStoryFromStoryMap(story);
        StoryMapStore.getStoryMap();
      });
    },
  },
  (connect, monitor) => ({
    connectDragSource: connect.dragSource(),
    isDragging: monitor.isDragging(),
  }),
)(StoryCard);
