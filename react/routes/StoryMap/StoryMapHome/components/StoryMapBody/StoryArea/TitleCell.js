import React, { Component, Fragment } from 'react';
import { observer } from 'mobx-react';
import { Icon, Tooltip } from 'choerodon-ui';
import SprintStatus from '@/components/tag/sprint-status-tag/SprintStatus';
import SprintAssigneeInfo from './SprintAssigneeInfo';
import Cell from '../Cell';
import StoryMapStore from '../../../../../../stores/project/StoryMap/StoryMapStore';
import './TitleCell.less';

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
          sprintId, sprintName, storyNum, statusCode, planning, assigneeIssues, todoStoryPoint, doingStoryPoint, doneStoryPoint,
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
            {
              statusCode ? (
                <Tooltip title={(
                  <div>
                    <p>{`待处理故事点：${todoStoryPoint || 0}`}</p>
                    <p>{`处理中故事点：${doingStoryPoint || 0}`}</p>
                    <p>{`已完成故事点：${doneStoryPoint || 0}`}</p>
                  </div>
                )}
                >
                  {`${sprintName} (${storyNum || 0})`}
                </Tooltip>
              ) : `${sprintName} (${storyNum || 0})`
            }
            {
              statusCode && (
                <SprintStatus data={{ statusCode, planning }} />
              )
            }
            {
              assigneeIssues && assigneeIssues.length > 0 && <SprintAssigneeInfo style={{ width: 0 }} assignees={assigneeIssues} />
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
            padding: '10px 0',
            background: 'white',
            ...showTitle ? {
              position: 'sticky',
              zIndex: 5,
              left: 0,
            // background: 'white',
            } : {},
            boxShadow: 'var(--divider) 0px -1px 0px inset',
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
