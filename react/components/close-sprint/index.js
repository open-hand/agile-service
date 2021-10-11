import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { Modal, Select } from 'choerodon-ui/pro';

import { stores } from '@choerodon/boot';
import { sprintApi } from '@/api';
import _ from 'lodash';

const { AppState } = stores;
const { Option } = Select;

@observer
class CloseSprint extends Component {
  constructor(props) {
    super(props);
    this.state = {
      selectChose: '0',
    };
  }

  componentDidMount() {
    const { modal } = this.props;
    modal.handleOk(this.handleCloseSprint);
  }

  /**
   *完成冲刺事件
   *
   * @memberof CloseSprint
   */
  handleCloseSprint = () => {
    const { selectChose } = this.state;
    const {
      sprintId, modal, afterClose,
    } = this.props;
    const data = {
      incompleteIssuesDestination: selectChose,
      projectId: AppState.currentMenuType.id,
      sprintId,
    };
    sprintApi.complete(data).then((res) => {
      modal.close();
      // console.log('completed');
      if (afterClose) {
        afterClose();
      }
    }).catch((error) => {
    });
  }

  render() {
    const { completeMessage, defaultValuePrompt } = this.props;
    const { selectChose } = this.state;
    return (
      <div className="c7n-pro-form-float">
        <p className="c7n-closeSprint-message">
          <span>{!_.isNull(completeMessage) ? completeMessage.partiallyCompleteIssues : ''}</span>
          {' '}
          个工作项 已经完成
        </p>
        <p style={{ marginTop: 14 }} className="c7n-closeSprint-message">
          <span>{!_.isNull(completeMessage) ? completeMessage.incompleteIssues : ''}</span>
          {' '}
          个工作项 未完成
        </p>
        {completeMessage?.parentsDoneUnfinishedSubtasks?.length ? (
          <p style={{ marginTop: 14, color: 'var(--text-color3)' }}>{`其中有${completeMessage ? completeMessage.parentsDoneUnfinishedSubtasks.length : 0}个已完成的工作项下有未完成的子任务，父级任务移动后与之相关的子任务也会被移动`}</p>
        ) : null}
        <div style={{ fontSize: 14, marginTop: 30 }}>选择该冲刺未完成的工作项移动到：</div>
        <Select
          label="移动至"
          labelLayout="float"
          style={{ marginTop: 12, width: 512 }}
          value={selectChose}
          onChange={(value) => {
            this.setState({
              selectChose: value,
            });
          }}
        >
          {!_.isNull(completeMessage) ? (
            completeMessage.sprintNames.map((item) => (
              <Option value={item.sprintId}>{item.sprintName}</Option>
            ))
          ) : ''}
          <Option value="0">待办事项</Option>
        </Select>
        {defaultValuePrompt && <p style={{ marginTop: 10, color: 'var(--text-color3)' }}>{defaultValuePrompt}</p>}
      </div>
    );
  }
}

export default function (props) {
  Modal.open({
    key: 'sprint',
    title: '完成冲刺',
    drawer: true,
    style: {
      width: 740,
    },
    children: <CloseSprint {...props} />,
  });
}
