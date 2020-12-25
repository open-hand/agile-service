import React, { Component, Fragment } from 'react';
import PropTypes from 'prop-types';
import { toJS } from 'mobx';
import { observer } from 'mobx-react';
import { Icon, Button, Tooltip } from 'choerodon-ui';
import StoryColumn from './StoryColumn';
import Cell from '../Cell';
import StoryMapStore from '../../../../../../stores/project/StoryMap/StoryMapStore';
import { ColumnMinHeight } from '../../../Constants';
import './StoryCell.less';

@observer
class StoryCell extends Component {
  getStorys = (targetFeature) => {
    const { swimLine } = StoryMapStore;
    const { version, sprint } = this.props;
    try {
      switch (swimLine) {
        case 'none': {
          return targetFeature.storys;
        }
        case 'version': {
          return targetFeature.version[version.versionId];
        }
        case 'sprint': {
          return targetFeature.sprint[sprint.sprintId];
        }
        default: return [];
      }
    } catch (error) {
      return [];
    }
  }

  render() {
    const {
      epic, otherData, storyCollapse, isLastRow, isLastColumn, epicIndex, lastCollapse,
    } = this.props;
    const { storyData, swimLine } = StoryMapStore;
    const { issueId: epicId, featureCommonDTOList, adding } = epic;
    const targetEpic = storyData[epicId] || {};
    const { collapse } = otherData || {};
    // let epicStorys = [];
    // if (targetEpic && targetEpic.feature && targetEpic.feature.none) {
    //   epicStorys = targetEpic.feature.none.storys;
    // }
    // const featureList = epicStorys.length > 0 ? featureCommonDTOList.concat([{ issueId: 'none' }]) : featureCommonDTOList;
    // 没有史诗不显示直接关联史诗的列
    const featureList = epicId === 0 ? featureCommonDTOList : featureCommonDTOList.concat([{ issueId: 'none' }]);
    return (
      !storyCollapse && !collapse && (
        <Cell epicIndex={epicIndex} lastCollapse={lastCollapse} collapse={collapse}>
          {collapse ? null : (
            <div style={{
              minHeight: ColumnMinHeight, height: '100%', display: 'flex', flexDirection: 'column',
            }}
            >
              <div style={{ display: 'flex', flex: 1 }}>
                {
                  adding ? null : (
                    <>
                      {featureList.filter((feature) => !feature.adding && feature.issueId).map((feature, index) => {
                        const targetFeature = targetEpic.feature[feature.issueId] || {};
                        if (targetFeature) {
                          const storys = this.getStorys(targetFeature) || [];
                          return (
                            (!StoryMapStore.hiddenColumnNoStory || (targetFeature.storys || []).length > 0)
                              ? (
                                <StoryColumn
                                  feature={feature}
                                  featureIndex={index}
                                  isLast={isLastColumn && index === featureList.length - 1}
                                  storys={storys}
                                  width={targetFeature.width}
                                  {...this.props}
                                />
                              ) : ''
                          );
                        }
                        return null;
                      })}
                    </>
                  )
                }
              </div>
            </div>
          )}
        </Cell>
      )
    );
  }
}

StoryCell.propTypes = {

};

export default StoryCell;
