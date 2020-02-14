import React, { Component, Fragment } from 'react';
import { observer } from 'mobx-react';
import {
  Tooltip, Icon,
} from 'choerodon-ui';
import classnames from 'classnames';
import { Button } from 'choerodon-ui/pro';
import UserHead from '@/components/UserHead';
import BacklogStore from '@/stores/project/backlog/BacklogStore';

import AssigneeModal from './AssigneeModal';
import './AssigneeInfo.less';

@observer class AssigneeInfo extends Component {
  constructor(props) {
    super(props);
    this.state = {
      expand: false,
    };
  }

  expandMore = () => {
    this.setState({
      expand: true,
    });
  };

  closeMore = () => {
    this.setState({
      expand: false,
    });
  };

  handleSearchAssignee = (assigneeId) => {
    const { data: { sprintId } } = this.props;
    const filterSprintAssignId = BacklogStore.filterSprintAssign.get(sprintId);
    if (filterSprintAssignId === assigneeId) {
      BacklogStore.clearFilterSprintAssign(sprintId);
    } else {
      BacklogStore.setFilterSprintAssign(sprintId, assigneeId);
    }
  };

  /**
 * 清除经办人筛选 
 */
  handleClearAssignee = () => {
    const { data: { sprintId } } = this.props;
    BacklogStore.clearFilterSprintAssign(sprintId);
  };

  render() {
    const { data } = this.props;
    const { assigneeIssues } = data;
    const { expand } = this.state;
    const filterSprintAssignId = BacklogStore.filterSprintAssign.get(data.sprintId);
    return (
      <Fragment>
        <div className="c7n-backlog-assignInfo">
          <div className="c7n-backlog-assignInfo-left">
            {assigneeIssues ? assigneeIssues
              .filter(assignee => assignee.assigneeId)
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
            <Icon
              style={{
                cursor: 'pointer',
                fontSize: 20,
                marginLeft: 8,
                display: assigneeIssues && assigneeIssues.length > 0 ? 'inline-block' : 'none',
              }}
              type="more_vert"
              role="none"
              onClick={this.expandMore}
            />

            {filterSprintAssignId && <Button color="blue" onClick={this.handleClearAssignee}>清除筛选</Button>}
          </div>
        </div>
        <AssigneeModal
          visible={expand}
          onCancel={this.closeMore}
          data={data}
        />
      </Fragment>
    );
  }
}

export default AssigneeInfo;
