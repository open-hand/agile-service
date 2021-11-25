import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { FlatSelect } from '@choerodon/components';
import { C7NFormat } from '@choerodon/master';
import StoryMapStore from '@/stores/project/StoryMap/StoryMapStore';

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
          marginLeft: 16, fontWeight: 500, marginRight: 8,
        }}
        onChange={this.handleSwitchSwinLine}
        clearButton={false}
      >
        <Option key="none" value="none">
          <C7NFormat
            intlPrefix="agile.storyMap"
            id="no.swimlane"
          />
        </Option>
        <Option key="version" value="version">
          <C7NFormat
            intlPrefix="agile.storyMap"
            id="version.swimlane"
          />

        </Option>
        <Option key="sprint" value="sprint">
          <C7NFormat
            intlPrefix="agile.storyMap"
            id="sprint.swimlane"
          />
        </Option>
      </FlatSelect>
    );
  }
}

SwitchSwimLine.propTypes = {

};

export default SwitchSwimLine;
