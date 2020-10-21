import React, { Component, Fragment } from 'react';
import { observer } from 'mobx-react';
import {
  Tooltip, Icon,
} from 'choerodon-ui';
import { findIndex } from 'lodash';
import classnames from 'classnames';
import { Button } from 'choerodon-ui/pro';
import UserHead from '@/components/UserHead';
import BacklogStore from '@/stores/project/backlog/BacklogStore';
import { localPageCacheStore } from '@/stores/common/LocalPageCacheStore';
import AssigneeModal from './AssigneeModal';
import './AssigneeInfo.less';

@observer class AssigneeInfo extends Component {
  constructor(props) {
    super(props);
    const { data: { sprintId, assigneeIssues } } = this.props;
    const assigneeId = localPageCacheStore.getItem(`backlog.sprint-${sprintId}`);
    if (assigneeId && findIndex(assigneeIssues, (item) => String(item.assigneeId) === String(assigneeId)) !== -1) {
      BacklogStore.setFilterSprintAssign(sprintId, assigneeId);
    } else {
      localPageCacheStore.remove(`backlog.sprint-${sprintId}`);
    }
  }

  handleSearchAssignee = (assigneeId) => {
    const { data: { sprintId } } = this.props;
    const filterSprintAssignId = BacklogStore.filterSprintAssign.get(sprintId);
    if (filterSprintAssignId === assigneeId) {
      localPageCacheStore.remove(`backlog.sprint-${sprintId}`);
      BacklogStore.clearFilterSprintAssign(sprintId);
    } else {
      localPageCacheStore.setItem(`backlog.sprint-${sprintId}`, assigneeId);
      BacklogStore.setFilterSprintAssign(sprintId, assigneeId);
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
                <UserHead
                  key={assigneeId}
                  title={(
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
                        {'问题: '}
                        {issueCount}
                      </p>
                    </div>
                    )}
                  hiddenText
                  className={classnames({
                    'c7n-backlog-assignInfo-item': true,
                    'c7n-backlog-assignInfo-item-active': filterSprintAssignId === assigneeId,
                  })}
                  onClick={() => this.handleSearchAssignee(assigneeId)}
                  size={24}
                  user={{
                    id: assigneeId,
                    loginName: assigneeLoginName,
                    realName: assigneeRealName,
                    name: assigneeName,
                    avatar: imageUrl,
                  }}
                />
              )) : null}
          </div>
          <div className="c7n-backlog-assignInfo-right">
            {filterSprintAssignId && <Button color="blue" onClick={this.handleClearAssignee}>清除筛选</Button>}
          </div>
        </div>

      </>
    );
  }
}

export default AssigneeInfo;
