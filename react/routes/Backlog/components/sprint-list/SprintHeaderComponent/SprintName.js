import React, { Component } from 'react';
import { TextField } from 'choerodon-ui/pro';
import { observer } from 'mobx-react';
import { Choerodon } from '@choerodon/boot';
import BacklogStore from '@/stores/project/backlog/BacklogStore';
import { sprintApi } from '@/api';
import TextEditToggle from '@/components/TextEditTogglePro';
import { MAX_LENGTH_SPRINT } from '@/constants/MAX_LENGTH';
import { getProjectId } from '@/utils/common';
import './SprintName.less';

@observer
class SprintName extends Component {
  validateSprintName = async (value, callback) => {
    const { data: { sprintName } } = this.props;
    if (value === sprintName) {
      return true;
    }
    const hasSame = await sprintApi.validate(value);
    if (hasSame) {
      return '已有同名冲刺';
    }
    return true;
  }

  handleBlurName=(value) => {
    if (/[^\s]+/.test(value)) {
      const { data } = this.props;
      const { objectVersionNumber, sprintId } = data;
      const req = {
        objectVersionNumber,
        projectId: getProjectId(),
        sprintId,
        sprintName: value,
      };
      sprintApi.updateSprint(req).then((res) => {
        if (res.failed) {
          Choerodon.prompt('冲刺名称已存在');
          return;
        }
        BacklogStore.updateSprint(sprintId, {
          sprintName: value,
          objectVersionNumber: res.objectVersionNumber,
        });
      }).catch((error) => {
      });
    }
  }

  render() {
    const {
      data: { sprintName, type }, noPermission,
    } = this.props;

    return (
      <div className="c7n-backlog-sprintName">
        <TextEditToggle
          disabled={type === 'backlog' || noPermission}
          onSubmit={this.handleBlurName}
          initValue={sprintName}
          style={{ whiteSpace: 'nowrap' }}
          editor={() => (
            <TextField
              maxLength={MAX_LENGTH_SPRINT}
              validator={this.validateSprintName}
            />
          )}
        >
          {sprintName}
        </TextEditToggle>
      </div>
    );
  }
}

export default SprintName;
