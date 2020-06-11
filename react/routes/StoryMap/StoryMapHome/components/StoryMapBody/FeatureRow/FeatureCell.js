import React, { Component, Fragment } from 'react';
import PropTypes from 'prop-types';
import { observer } from 'mobx-react';
import { find } from 'lodash';
import FeatureColumn from './FeatureColumn';
import Cell from '../Cell';
import StoryMapStore from '../../../../../../stores/project/StoryMap/StoryMapStore';

@observer
class FeatureCell extends Component {
  handleAddFeatureClick=() => {
    const { epic, otherData } = this.props;
    const { epicId } = epic;
    StoryMapStore.addFeature(epic);
  }

  handleCreateFeature=(newFeature) => {
    const { epicIndex } = this.props;
    StoryMapStore.afterCreateFeature(epicIndex, newFeature);
  }

  getStorys = (targetFeature) => {
    const { swimLine } = StoryMapStore;
    const { version } = this.props;
    try {
      switch (swimLine) {
        case 'none': {
          return targetFeature.storys;
        }
        case 'version': {
          return targetFeature.version[version.versionId];
        }
        default: return [];
      }
    } catch (error) {
      return [];
    }
  }

  render() {
    const {
      epic, otherData, isLastColumn, lastCollapse, epicIndex,
    } = this.props;
    const { issueId: epicId, featureCommonDTOList, adding } = epic;
    const { storyData, swimLine } = StoryMapStore;
    const targetEpic = storyData[epicId] || {};
    const { collapse } = otherData || {};
    return (
      (collapse || (StoryMapStore.hiddenColumnNoStory && otherData.storys.length === 0)) ? null : (
        <Cell
          epicIndex={epicIndex}
          lastCollapse={lastCollapse}
          collapse={collapse}
          style={{
            position: 'sticky',
            top: 82,
            zIndex: 6,
            background: 'white',
            ...collapse ? { zIndex: 'unset' } : {}, 
          }}
        >
          { (
            <div style={{ display: 'flex' }}>        
              {adding ? null : (
                <Fragment>
                  {featureCommonDTOList.filter(feature => !feature.adding).map(feature => {
                    const targetFeature = targetEpic.feature[feature.issueId] || {};
                    if (targetFeature) {
                      const storys = this.getStorys(targetFeature);
                      return (
                        (!StoryMapStore.hiddenColumnNoStory || storys.length > 0) ? <FeatureColumn epic={epic} feature={feature} otherData={otherData ? otherData.feature[feature.issueId] : {}} /> : ''
                      )
                    } else {
                      return null;
                    }
                  })}             
                  {/* 没有关联feature，但是关联了史诗的故事 */}
                  {otherData && otherData.feature.none && otherData.feature.none.storys.length > 0 ? <FeatureColumn isLast={isLastColumn} epic={epic} feature={{ issueId: 'none' }} otherData={otherData.feature.none} /> : null}                  
                </Fragment>
              )}
            </div>
        )}
        </Cell>
      )
    );
  }
}

FeatureCell.propTypes = {

};

export default FeatureCell;
