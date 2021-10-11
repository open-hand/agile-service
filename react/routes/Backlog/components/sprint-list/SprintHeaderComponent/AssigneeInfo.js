import React, { Component, Fragment } from 'react';
import { observer } from 'mobx-react';
import { find } from 'lodash';
import classnames from 'classnames';
import { Button } from 'choerodon-ui/pro';
import BacklogStore from '@/stores/project/backlog/BacklogStore';
import { localPageCacheStore } from '@/stores/common/LocalPageCacheStore';
import './AssigneeInfo.less';
import { UserUniqueTag } from '@/components/tag/user-tag';

@observer class AssigneeInfo extends Component {
  handleSearchAssignee = (assigneeId) => {
    const { data: { sprintId, assigneeIssues } } = this.props;
    const filterSprintAssignId = BacklogStore.filterSprintAssign.get(sprintId);
    if (filterSprintAssignId === assigneeId) {
      localPageCacheStore.remove(`backlog.sprint-${sprintId}`);
      BacklogStore.clearFilterSprintAssign(sprintId);
    } else {
      localPageCacheStore.setItem(`backlog.sprint-${sprintId}`, assigneeId);
      BacklogStore.setFilterSprintAssign(sprintId, assigneeId);
      const sprintDefaultAssignee = find(assigneeIssues, (item) => String(item.assigneeId) === String(assigneeId));
      BacklogStore.setFilterSprintAssignUser(sprintId, {
        id: assigneeId,
        imageUrl: sprintDefaultAssignee.imageUrl,
        loginName: sprintDefaultAssignee.assigneeLoginName,
        realName: sprintDefaultAssignee.assigneeRealName,
        tooltip: sprintDefaultAssignee.assigneeName,
      });
    }
  };

  /**
 * 清除经办人筛选
 */
  handleClearAssignee = () => {
    const { data: { sprintId } } = this.props;
    localPageCacheStore.remove(`backlog.sprint-${sprintId}`);
    BacklogStore.clearFilterSprintAssign(sprintId);
  };

  render() {
    const { data } = this.props;
    const { assigneeIssues } = data;
    const filterSprintAssignId = BacklogStore.filterSprintAssign.get(data.sprintId);
    return (
      <>
        <div className="c7n-backlog-assignInfo">
          <div className="c7n-backlog-assignInfo-left">
            {assigneeIssues ? assigneeIssues
              .filter((assignee) => assignee.assigneeId)
              .map(({
                assigneeId,
                assigneeName,
                totalStoryPoints,
                totalRemainingTime,
                issueCount,
                assigneeLoginName,
                assigneeRealName,
                imageUrl,
              }) => (
                <UserUniqueTag
                  key={assigneeId}
                  showText={false}
                  className={classnames({
                    'c7n-backlog-assignInfo-item': true,
                    'c7n-backlog-assignInfo-item-active': filterSprintAssignId === assigneeId,
                  })}
                  onClick={() => this.handleSearchAssignee(assigneeId)}
                  size={24}
                  data={{
                    id: assigneeId,
                    loginName: assigneeLoginName,
                    realName: assigneeRealName,
                    name: assigneeName,
                    tooltip: (
                      <div>
                        <p>{assigneeName}</p>
                        <p>
                          {'故事点: '}
                          {totalStoryPoints || 0}
                        </p>
                        <p>
                          {'剩余预估时间: '}
                          {totalRemainingTime || '无'}
                        </p>
                        <p>
                          {'工作项: '}
                          {issueCount}
                        </p>
                      </div>),
                    imageUrl,
                  }}
                />
              )) : null}
          </div>
          <div className="c7n-backlog-assignInfo-right">
            {filterSprintAssignId && <Button onClick={this.handleClearAssignee}>清除筛选</Button>}
          </div>
        </div>

      </>
    );
  }
}

export default AssigneeInfo;
