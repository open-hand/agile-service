/* eslint-disable react/prop-types */
import React, { Component, Fragment } from 'react';
import { observer } from 'mobx-react';
import FeatureColumn from './FeatureColumn';
import Cell from '../Cell';
import StoryMapStore from '../../../../../../stores/project/StoryMap/StoryMapStore';

@observer
class FeatureCell extends Component {
  handleAddFeatureClick=() => {
    const { epic } = this.props;
    StoryMapStore.addFeature(epic);
  }

  handleCreateFeature=(newFeature) => {
    const { epicIndex } = this.props;
    StoryMapStore.afterCreateFeature(epicIndex, newFeature);
  }

  getStorys = (targetFeature) => targetFeature.storys || []

  render() {
    const {
      epic, otherData, isLastColumn, lastCollapse, epicIndex,
    } = this.props;
    const { issueId: epicId, featureCommonDTOList, adding } = epic;
    const { storyData } = StoryMapStore;
    const targetEpic = storyData[epicId] || {};
    const { collapse } = otherData || {};
    // 无特性的故事不会显示在板子上,当隐藏无故事的列时，隐藏特性列
    // const storysWithFeature = (otherData.storys || []).filter((item) => item.featureId && item.featureId !== '0');
    return (
      (collapse || (StoryMapStore.hiddenColumnNoStory && otherData.storys?.length === 0)) ? null : (
        <Cell
          epicIndex={epicIndex}
          lastCollapse={lastCollapse}
          collapse={collapse}
          style={{
            position: 'sticky',
            top: 97,
            zIndex: 6,
            background: 'white',
            ...collapse ? { zIndex: 'unset' } : {},
          }}
        >
          <div style={{ display: 'flex' }}>
            {adding ? null : (
              <>
                {featureCommonDTOList.filter((feature) => !feature.adding).map((feature) => {
                  const targetFeature = targetEpic.feature[feature.issueId] || {};
                  if (targetFeature) {
                    const storys = this.getStorys(targetFeature);
                    return (
                      (!StoryMapStore.hiddenColumnNoStory || storys.length > 0) ? <FeatureColumn epic={epic} feature={feature} otherData={otherData ? otherData.feature[feature.issueId] : {}} /> : ''
                    );
                  }
                  return null;
                })}
                {/* 没有关联feature，但是关联了史诗的故事 */}
                {otherData && otherData.feature.none && otherData.feature.none.storys.length > 0 ? <FeatureColumn isLast={isLastColumn} epic={epic} feature={{ issueId: 'none' }} otherData={otherData.feature.none} /> : null}
              </>
            )}
          </div>
        </Cell>
      )
    );
  }
}

FeatureCell.propTypes = {

};

export default FeatureCell;
