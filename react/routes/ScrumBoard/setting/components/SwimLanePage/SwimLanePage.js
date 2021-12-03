import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { C7NFormat } from '@choerodon/master';
import { Content, Choerodon } from '@choerodon/boot';
import { Select } from 'choerodon-ui/pro';
import ScrumBoardStore from '@/stores/project/scrumBoard/ScrumBoardStore';
import { boardApi } from '@/api';

const { Option } = Select;

@observer
class SwimLanePage extends Component {
  constructor(props) {
    super(props);
    this.state = {
      selectValue: '',
    };
  }

  handleSave(select) {
    const { selectValue, selectedValue } = this.state;

    boardApi.updateUserSetting(select.boardId, selectValue || ScrumBoardStore.getSwimLaneCode).then((res) => {
      ScrumBoardStore.setSwimLaneCode(selectedValue);
      Choerodon.prompt('保存成功');
    }).catch((error) => {
      Choerodon.prompt('保存失败');
    });
  }

  render() {
    const defaultSelect = ScrumBoardStore.getBoardList.get(ScrumBoardStore.getSelectedBoard);
    return (
      <div>
        <Select
          style={{ width: 512 }}
          label={(
            <C7NFormat
              intlPrefix="agile.scrumBoard"
              id="board.swimlane.in"
            />
          )}
          clearButton={false}
          labelLayout="float"
          defaultValue={ScrumBoardStore.getSwimLaneCode || 'parent_child'}
          onChange={(value) => {
            this.setState({
              selectValue: value,
            }, () => {
              this.handleSave(defaultSelect);
            });
          }}
        >
          <Option value="parent_child">
            <C7NFormat
              intlPrefix="agile.common"
              id="story"
            />
          </Option>
          <Option value="assignee">
            <C7NFormat
              intlPrefix="agile.common"
              id="assignee"
            />
          </Option>
          <Option value="participant">
            <C7NFormat
              intlPrefix="agile.systemField"
              id="participant"
            />
          </Option>
          <Option value="swimlane_epic">
            <C7NFormat
              intlPrefix="agile.common"
              id="epic"
            />
          </Option>
          <Option value="swimlane_none">
            <C7NFormat
              intlPrefix="agile.common"
              id="none"
            />

          </Option>
        </Select>
      </div>
    );
  }
}

export default SwimLanePage;
