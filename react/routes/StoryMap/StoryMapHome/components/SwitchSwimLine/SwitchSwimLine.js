import React, { Component } from 'react';
import { observer } from 'mobx-react';
import StoryMapStore from '@/stores/project/StoryMap/StoryMapStore';
import { FlatSelect } from '@choerodon/components';

const { Option } = FlatSelect;
@observer
class SwitchSwimLine extends Component {
  handleSwitchSwinLine = (key) => {
    StoryMapStore.switchSwimLine(key);
  }

  render() {
    const { swimLine } = StoryMapStore;

    return (
      <FlatSelect
        value={swimLine}
        style={{
          marginRight: 15, fontWeight: 500,
        }}
        onChange={this.handleSwitchSwinLine}
        clearButton={false}
      >
        <Option key="none" value="none">无泳道</Option>
        <Option key="version" value="version">版本泳道</Option>
        <Option key="sprint" value="sprint">冲刺泳道</Option>
      </FlatSelect>
    );
  }
}

SwitchSwimLine.propTypes = {

};

export default SwitchSwimLine;
