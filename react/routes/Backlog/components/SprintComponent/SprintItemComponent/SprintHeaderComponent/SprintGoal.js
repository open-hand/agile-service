import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import { Input } from 'choerodon-ui';
import TextEditToggle from '@/components/TextEditToggle';

const { Text, Edit } = TextEditToggle;

@inject('AppState', 'HeaderStore')
@observer class SprintGoal extends Component {
  handler = (value) => {
    const { handleChangeGoal } = this.props;
    handleChangeGoal(value);
  };

  render() {
    const { sprintGoal } = this.props;
    return (
      <div
        style={{
          display: 'flex',
          alignItems: 'center',
          minWidth: '100px',
          justifyContent: 'flex-end',
        }}
      >
        <p
          style={{ whiteSpace: 'nowrap' }}
        >
          {'冲刺目标：'}
        </p>
        <TextEditToggle
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
