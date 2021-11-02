import React, { Component, Fragment } from 'react';
import { observer } from 'mobx-react';
import { toJS } from 'mobx';
import StoryMapStore from '@/stores/project/StoryMap/StoryMapStore';
import StoryCell from './StoryCell';
import TitleCell from './TitleCell';

@observer
class StoryRow extends Component {
  getFirstNotCollapseIndex = () => {
    const { storyData } = StoryMapStore;
    const epicList = StoryMapStore.getEpicList;
    for (let i = 0; i < epicList.length; i += 1) {
      if (!storyData[epicList[i].issueId].collapse) {
        return i;
      }
    }
    return 0;
  }

  render() {
    const {
      storyMapData, storyData, swimLine, tableOverflow,
    } = StoryMapStore;
    const { epicWithFeature } = storyMapData || {};
    const epicList = StoryMapStore.getEpicList;
    const firstNotCollapseIndex = this.getFirstNotCollapseIndex();
    const {
      storyCollapse, rowIndex, sprint, version,
    } = this.props;
    const { versionId } = version || {};
    const {
      sprintId, statusCode,
    } = sprint || {};
    return (
      <>
        {/* 标题行 */}
        {['version', 'sprint'].includes(swimLine) && (
        <tr className={versionId || sprintId ? `row-${versionId || sprintId}` : ''} style={{ borderRight: '1px solid rgba(211, 211, 211)' }}>
          {epicList.map((epic, index, arr) => (
            <TitleCell
              epicIndex={index}
              isLastColumn={index === epicWithFeature.length - 1}
              lastCollapse={index > 0 ? storyData[epicList[index - 1].issueId] && storyData[epicList[index - 1].issueId].collapse : false}
              showTitle={firstNotCollapseIndex === index}
              otherData={storyData[epic.issueId]}
              {...this.props}
            />
          ))}
        </tr>
        )}
        <tr style={{ ...storyCollapse ? { height: 0 } : {} }}>
          {epicList.map((epic, index) => {
            const otherData = storyData[epic.issueId];
            // 无特性的故事不会显示在板子上,当隐藏无故事的列时，隐藏特性列
            // const storysWithFeature = ((otherData && otherData.storys) || []).filter((item) => item.featureId && item.featureId !== '0');
            return (!StoryMapStore.hiddenColumnNoStory || otherData.storys?.length > 0) ? (
              <StoryCell
                rowIndex={rowIndex}
                epicIndex={index}
                lastCollapse={index > 0 ? storyData[epicList[index - 1].issueId] && storyData[epicList[index - 1].issueId].collapse : false}
                isLastColumn={index === epicWithFeature.length - 1}
                showTitle={firstNotCollapseIndex === index}
                epic={epic}
                otherData={storyData[epic.issueId]}
                {...this.props}
              />
            ) : '';
          })}
        </tr>
      </>
    );
  }
}

StoryRow.propTypes = {

};

export default StoryRow;
