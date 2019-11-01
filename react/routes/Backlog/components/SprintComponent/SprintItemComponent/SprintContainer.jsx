import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import { Choerodon } from '@choerodon/boot';
import classnames from 'classnames';
import SprintHeader from './SprintHeader';
import SprintBody from './SprintBody';
import NoneIssue from './NoneIssue';
import NoneBacklog from './NoneBacklog';
import BacklogHeader from './BacklogHeader';
import BacklogStore from '../../../../../stores/project/backlog/BacklogStore';

const shouldContainTypeCode = ['issue_epic', 'sub_task', 'feature'];

@inject('AppState')
@observer class SprintContainer extends Component {
  constructor(props) {
    super(props);
    this.state = {
      expand: true,
      isVisibleClearBtn: false, // 是否显示清除经办人筛选按钮
    };
    this.strategyMap = new Map([
      ['sprint', this.renderSprint],
      ['backlog', this.renderBacklog],
    ]);
  }

  componentDidMount() {
    const { isCreated } = this.props;
    if (isCreated) {
      setTimeout(() => {
        this.ref.scrollIntoView();
        this.ref.style.background = 'white';
      }, 10, this);
    }
  }

  toggleSprint = () => {
    const { expand } = this.state;
    this.setState({
      expand: !expand,
    });
  };

  renderBacklog = (backlogData) => {
    const { expand } = this.state;
    const issueCount = BacklogStore.getIssueMap.get('0') ? BacklogStore.getIssueMap.get('0').length : 0;
    return (
      <div
        // eslint-disable-next-line no-return-assign
        ref={e => this.ref = e}
        style={{
          // background: isCreated ? '#eee' : 'white',
          transition: 'all 2s',
          width: '100%',
        }}
      // key={sprintItem.sprintId}
      >
        <BacklogHeader
          issueCount={issueCount}
          data={backlogData}
          expand={expand}
          sprintId="0"
          toggleSprint={this.toggleSprint}
        />
        <SprintBody
          issueType={BacklogStore.getIssueTypes.filter(type => shouldContainTypeCode.indexOf(type.typeCode) === -1)}
          defaultType={BacklogStore.getIssueTypes.find(type => type.typeCode === 'story')}
          defaultPriority={BacklogStore.getDefaultPriority}
          issueCount={!!issueCount}
          expand={expand}
          sprintId="0"
          droppableId="backlogData"
          EmptyIssueComponent={NoneBacklog}
        />
      </div>
    );
  };

  /**
 * 经办人搜索
  * filter 经办人ID
 */
  handleSearchAssignee = (sprintId, filter) => {
    // 设置经办人过滤条件函数
    const { onAssigneeChange } = this.props;
    if (onAssigneeChange && filter) {
      onAssigneeChange({
        assigneeId: filter[0],
        sprintId,
      });
      this.setState({ isVisibleClearBtn: true });
    } else if (!filter) {
      onAssigneeChange({});
      this.setState({ isVisibleClearBtn: false });
    } else {
      Choerodon.prompt('经办人筛选错误');
    }
  };

  renderSprint = (sprintItem) => {
    const { refresh, isCreated } = this.props;
    const { expand, isVisibleClearBtn } = this.state;
    const issueCount = BacklogStore.getIssueMap.get(sprintItem.sprintId.toString()) ? BacklogStore.getIssueMap.get(sprintItem.sprintId.toString()).length : 0;
    return (
      <div
        // eslint-disable-next-line no-return-assign
        ref={e => this.ref = e}
        style={{
          background: isCreated ? '#eee' : 'white',
          transition: 'all 2s',
          width: '100%',
        }}
        key={sprintItem.sprintId}
      >
        <SprintHeader
          refresh={refresh}
          issueCount={issueCount}
          data={sprintItem}
          expand={expand}
          isVisibleClearBtn={isVisibleClearBtn}
          handleSearchAssignee={this.handleSearchAssignee.bind(this, sprintItem.sprintId)}
          sprintId={sprintItem.sprintId.toString()}
          toggleSprint={this.toggleSprint}
        />
        <SprintBody
          issueType={BacklogStore.getIssueTypes.filter(type => shouldContainTypeCode.indexOf(type.typeCode) === -1)}
          defaultType={BacklogStore.getIssueTypes.find(type => type.typeCode === 'story')}
          defaultPriority={BacklogStore.getDefaultPriority}
          issueCount={!!issueCount}
          expand={expand}
          sprintId={sprintItem.sprintId.toString()}
          droppableId={sprintItem.sprintId.toString()}
          EmptyIssueComponent={NoneIssue}
        />
      </div>
    );
  };

  render() {
    const { data, type } = this.props;
    return this.strategyMap.get(type)(data);
  }
}

export default SprintContainer;
