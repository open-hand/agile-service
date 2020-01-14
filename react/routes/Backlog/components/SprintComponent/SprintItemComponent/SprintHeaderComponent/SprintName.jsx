import React, { Component } from 'react';
import { Icon, Input } from 'choerodon-ui';
import { checkSprintName } from '@/api/NewIssueApi';
import TextEditToggle from '@/components/TextEditToggle';
import { SPRINT_MAX_LENGTH } from '@/common/Constant';

const { Text, Edit } = TextEditToggle;

class SprintName extends Component {
  validateSprintName=async (rule, value, callback) => {
    const { sprintName } = this.props;
    if (value === sprintName) {
      callback();
    } else {
      const hasSame = await checkSprintName(value);
      if (hasSame) {
        callback('');
      } else {
        callback();
      }
    }
  }

  render() {
    const {
      expand, sprintName, toggleSprint, type, handleBlurName,
    } = this.props;
    return (
      <div className="c7n-backlog-sprintName">
        <Icon
          style={{ fontSize: 20, cursor: 'pointer' }}
          type={expand ? 'baseline-arrow_drop_down' : 'baseline-arrow_right'}
          role="none"
          onClick={toggleSprint}
        />
        <TextEditToggle
          disabled={type === 'backlog'}
          formKey="sprint"
          onSubmit={handleBlurName}
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
            {({ width }) => <Input size="small" style={{ width: Math.max(width, 200), fontSize: 13 }} autoFocus maxLength={SPRINT_MAX_LENGTH} showLengthInfo={false} />}
          </Edit>
        </TextEditToggle>        
      </div>
    );
  }
}

export default SprintName;
