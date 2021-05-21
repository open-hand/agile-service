import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { Input } from 'choerodon-ui';
import TextEditToggle from '@/components/TextEditToggle';
import { getProjectId } from '@/utils/common';
import { sprintApi } from '@/api';
import BacklogStore from '@/stores/project/backlog/BacklogStore';

const { Text, Edit } = TextEditToggle;

@observer class SprintGoal extends Component {
  handler = (value) => {
    const { data } = this.props;
    const { objectVersionNumber, sprintId } = data;
    const req = {
      objectVersionNumber,
      projectId: getProjectId(),
      sprintId,
      sprintGoal: value,
    };
    sprintApi.updateSprint(req).then((res) => {
      BacklogStore.updateSprint(sprintId, {
        objectVersionNumber: res.objectVersionNumber,
        sprintGoal: res.sprintGoal,
      });
    }).catch((error) => {
    });
  };

  render() {
    const { data: { sprintGoal }, noPermission } = this.props;
    return (
      <div
        style={{
          display: 'flex',
          alignItems: 'center',
          minWidth: '100px',
          justifyContent: 'flex-end',
          marginLeft: 'auto',
        }}
      >
        <p
          style={{ whiteSpace: 'nowrap' }}
        >
          冲刺目标：
        </p>
        <TextEditToggle
          disabled={noPermission}
          formKey="goal"
          onSubmit={this.handler}
          originData={sprintGoal}
          style={{ whiteSpace: 'nowrap' }}
          className="hidden-length-info"
        >
          <Text>
            {sprintGoal || '无'}
          </Text>
          <Edit>
            {({ width }) => <Input size="small" style={{ width: Math.max(width, 300), fontSize: 13 }} autoFocus maxLength={30} />}
          </Edit>
        </TextEditToggle>
      </div>
    );
  }
}

export default SprintGoal;
