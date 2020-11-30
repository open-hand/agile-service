import React, { Component, Fragment } from 'react';
import { observer } from 'mobx-react';
import { Icon } from 'choerodon-ui';
import Cell from '../Cell';
import StoryMapStore from '../../../../../../stores/project/StoryMap/StoryMapStore';
import './TitleCell.less';
import { toJS } from 'mobx';
import SprintStatus from '@/components/tag/sprint-status-tag/SprintStatus';
import SprintAssigneeInfo from './SprintAssigneeInfo';

@observer
class TitleCell extends Component {
  renderTitle = (storyCollapse) => {
    const { swimLine } = StoryMapStore;
    const { version, sprint } = this.props;
    switch (swimLine) {
      case 'none': {
        return null;
      }
      case 'version': {
        return (
          <>
            <Icon
              style={{ marginRight: 15 }}
              type={storyCollapse ? 'expand_less' : 'expand_more'}
              onClick={(e) => {
                StoryMapStore.collapseStory(version.versionId);
              }}
            />
            {version.name}
            {` (${version.storyNum || 0})`}
          </>
        );
      }
      case 'sprint': {
        const {
          sprintId, sprintName, storyNum, statusCode, planning, assigneeIssues,
        } = sprint;
        return (
          <>
            <Icon
              style={{ marginRight: 15 }}
              type={storyCollapse ? 'expand_less' : 'expand_more'}
              onClick={(e) => {
                StoryMapStore.collapseStory(sprintId);
              }}
            />
            {sprintName}
            {` (${storyNum || 0})`}
            {
              statusCode && (
                <SprintStatus data={{ statusCode, planning }} />
              )
            }
            {
              assigneeIssues && assigneeIssues.length > 0 && <SprintAssigneeInfo assignees={assigneeIssues} />
            }
          </>
        );
      }
      default: return null;
    }
  }

  render() {
    const {
      otherData, showTitle, storyCollapse, epicIndex, isLastRow, lastCollapse,
    } = this.props;
    const { collapse } = otherData || {};

    return (
      collapse ? null : (
        <Cell
          style={{
          // borderBottom: storyCollapse ? '1px solid #D8D8D8' : 'none',
            padding: '10px 0',
            boxShadow: storyCollapse ? 'inset 0 -1px 0 #D8D8D8,inset 1px 0 0 #D8D8D8' : 'inset 1px 0 0 #D8D8D8',
            ...lastCollapse || epicIndex === 0 ? { boxShadow: storyCollapse ? 'inset 0 -1px 0 #D8D8D8' : 'none' } : {},
            ...showTitle ? {
              position: 'sticky',
              zIndex: 5,
              left: 0,
            // background: 'white',
            } : {},
          }}
        >
          <div style={{ display: 'flex' }} className="c7nagile-StoryMap-TitleCell">
            {showTitle && this.renderTitle(storyCollapse)}
          </div>
        </Cell>
      )
    );
  }
}

TitleCell.propTypes = {

};

export default TitleCell;
