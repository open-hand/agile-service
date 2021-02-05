import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { Modal } from 'choerodon-ui/pro';
import { Select } from 'choerodon-ui';
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
      <div>
        <p className="c7n-closeSprint-message">
          <span>{!_.isNull(completeMessage) ? completeMessage.partiallyCompleteIssues : ''}</span>
          {' '}
          个问题 已经完成
        </p>
        <p style={{ marginTop: 24 }} className="c7n-closeSprint-message">
          <span>{!_.isNull(completeMessage) ? completeMessage.incompleteIssues : ''}</span>
          {' '}
          个问题 未完成
        </p>
        <p style={{ marginTop: 19, color: 'rgba(0,0,0,0.65)' }}>{`其中有${completeMessage ? completeMessage.parentsDoneUnfinishedSubtasks.length : 0}个问题包含子任务，父级任务移动后与之相关的子任务也会被移动`}</p>
        <p style={{ fontSize: 14, marginTop: 36 }}>选择该冲刺未完成的问题：</p>
        <Select
          label="移动至"
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
        {defaultValuePrompt && <p style={{ marginTop: 10, color: 'rgba(0,0,0,0.65)' }}>{defaultValuePrompt}</p>}
      </div>
    );
  }
}

export default function (props) {
  Modal.open({
    key: 'sprint',
    title: '完成冲刺',
    okText: '结束',
    cancelText: '取消',
    drawer: true,
    style: {
      width: 740,
    },
    children: <CloseSprint {...props} />,
  });
}
