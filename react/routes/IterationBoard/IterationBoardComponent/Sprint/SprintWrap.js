import React, { Component } from 'react';
import Card from '../Card';
import Sprint from './Sprint';

class SprintWrap extends Component {
  render() {
    const { sprintId, sprintName } = this.props;

    return (
      <Card
        style={{ height: '100%' }}
        title={sprintName || <span style={{ visibility: 'hidden' }}>title</span>} // 保证头部高度
        sprintId={sprintId}
      >
        <Sprint
          sprintId={sprintId}
        />
      </Card>
    );
  }
}

export default SprintWrap;
