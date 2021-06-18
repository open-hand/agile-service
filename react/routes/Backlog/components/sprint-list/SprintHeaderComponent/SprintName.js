import React, { Component } from 'react';
import { Input } from 'choerodon-ui';
import { observer } from 'mobx-react';
import { Choerodon } from '@choerodon/boot';
import BacklogStore from '@/stores/project/backlog/BacklogStore';
import { sprintApi } from '@/api';
import TextEditToggle from '@/components/TextEditToggle';
import { MAX_LENGTH_SPRINT } from '@/constants/MAX_LENGTH';
import { getProjectId } from '@/utils/common';
import './SprintName.less';

const { Text, Edit } = TextEditToggle;

@observer
class SprintName extends Component {
  validateSprintName = async (rule, value, callback) => {
    const { data: { sprintName } } = this.props;
    if (value === sprintName) {
      callback();
    } else {
      const hasSame = await sprintApi.validate(value);
      if (hasSame) {
        callback('');
      } else {
        callback();
      }
    }
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
          formKey="sprint"
          onSubmit={this.handleBlurName}
          originData={sprintName}
          style={{ whiteSpace: 'nowrap' }}
          rules={[{
            validator: this.validateSprintName,
          }]}
          className="hidden-length-info"
        >
          <Text>
            {sprintName}
          </Text>
          <Edit>
            {({ width }) => <Input size="small" style={{ width: Math.max(width, 200), fontSize: 13 }} autoFocus maxLength={MAX_LENGTH_SPRINT} showLengthInfo={false} />}
          </Edit>
        </TextEditToggle>
      </div>
    );
  }
}

export default SprintName;
