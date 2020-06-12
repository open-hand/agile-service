import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { Draggable } from 'react-beautiful-dnd';
import { featureApi } from '@/api';
import BacklogStore from '../../../../stores/project/backlog/BacklogStore';
import DraggableFeature from './DraggableFeature';

@observer
class FeatureItem extends Component {
  /**
   *点击featureItem的事件
   *
   * @param {*} type
   * @memberof 
   */
  handleClickFeature = (type) => {
    const { clickFeature } = this.props;
    clickFeature(type);
  };


  render() {
    const { issueRefresh, refresh } = this.props;
    return (
      BacklogStore.getFeatureData.map((item, index) => (
        <div
          key={item.issueId}
          role="none"
          onMouseEnter={(e) => {
            if (BacklogStore.isDragging) {
              BacklogStore.toggleIssueDrag(true);
              e.currentTarget.style.border = '2px dashed green';
            }
          }}
          onMouseLeave={(e) => {
            if (BacklogStore.isDragging) {
              BacklogStore.toggleIssueDrag(false);
              e.currentTarget.style.border = 'none';
            }
          }}
          onMouseUp={(e) => {
            if (BacklogStore.getIsDragging) {
              BacklogStore.toggleIssueDrag(false);
              e.currentTarget.style.border = 'none';
              featureApi.addIssues(
                item.issueId, BacklogStore.getIssueWithEpicOrVersion,
              ).then((res) => {
                issueRefresh();
                refresh(true, false);
              }).catch((error) => {
                issueRefresh();
                refresh(true, false);
              });
            }
          }}
          onClick={(e) => {
            this.handleClickFeature(item.issueId);
          }}
        >
          <Draggable draggableId={String(item.issueId)} key={item.issueId} index={index}>
            {(draggableProvided, draggableSnapshot) => (
              <DraggableFeature
                item={item}
                refresh={refresh}
                draggableProvided={draggableProvided}
                draggableSnapshot={draggableSnapshot}
                handleClickFeature={this.handleClickFeature}
              />
            )}
          </Draggable>
        </div>
      ))
    );
  }
}

export default FeatureItem;
