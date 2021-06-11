import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { TextField } from 'choerodon-ui/pro';
import TextEditToggle from '@/components/TextEditTogglePro';
import { getProjectId } from '@/utils/common';
import { sprintApi } from '@/api';
import BacklogStore from '@/stores/project/backlog/BacklogStore';

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
          onSubmit={this.handler}
          initValue={sprintGoal}
          style={{ whiteSpace: 'nowrap' }}
          editor={() => <TextField autoFocus maxLength={30} style={{ width: 200 }} />}
        >
          {sprintGoal || '无'}
        </TextEditToggle>
      </div>
    );
  }
}

export default SprintGoal;
