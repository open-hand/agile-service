import React, { Component } from 'react';
import {
  Choerodon,
} from '@choerodon/boot';
import {
  Form, TextField, Button, Spin, DataSet, Divider,
} from 'choerodon-ui/pro';
import ScrumBoardStore from '@/stores/project/scrumBoard/ScrumBoardStore';
import { boardApi } from '@/api';

class EditBoardName extends Component {
  constructor(props) {
    super(props);
    this.state = {
      loading: false,
      initialBoardName: '',
      lastBoardName: '',
    };
    this.dataSet = new DataSet({
      autoCreate: false,
      fields: [{
        name: 'boardName',
        required: true,
        validator: this.checkBoardNameRepeat,
      }],
    });
  }

  componentDidMount() {
    const boardList = ScrumBoardStore.getBoardList;
    const initialBoardName = boardList.get(ScrumBoardStore.getSelectedBoard).name;
    this.setState({
      initialBoardName,
      lastBoardName: initialBoardName,
    });
    this.dataSet.create({
      boardName: initialBoardName,
    });
  }

  checkBoardNameRepeat = async (value) => {
    const { initialBoardName } = this.state;

    if (initialBoardName === value) {
      return true;
    }
    const res = await boardApi.checkName(value);
    if (res) {
      return '看板名称重复';
    }
    return true;
  };

  handleUpdateBoardName = async () => {
    const currentEditBoard = ScrumBoardStore.getBoardList.get(ScrumBoardStore.getSelectedBoard);
    const { objectVersionNumber, boardId, projectId } = currentEditBoard;

    if (await this.dataSet.validate()) {
      const boardName = this.dataSet.current.get('boardName');
      const data = {
        objectVersionNumber,
        boardId,
        name: boardName,
        projectId,
      };
      this.setState({
        loading: true,
        lastBoardName: this.dataSet.current.get('boardName'),
      });
      boardApi.update(data.boardId, data).then((res) => {
        this.setState({
          loading: false,
        });
        ScrumBoardStore.setBoardList(ScrumBoardStore.getSelectedBoard, res);
        Choerodon.prompt('保存成功');
      });
    }
  }

  render() {
    const {
      initialBoardName, loading, lastBoardName,
    } = this.state;
    const { editBoardNameDisabled } = this.props;
    return (
      <div>
        <Spin spinning={loading}>
          {
            editBoardNameDisabled ? (
              <TextField
                style={{ width: 512, marginTop: 5 }}
                label="看板名称"
                maxLength={10}
                value={initialBoardName}
                disabled
              />
            ) : (
              <div>
                <Form dataSet={this.dataSet} style={{ width: 512 }}>
                  <TextField
                    name="boardName"
                    label="看板名称"
                    maxLength={10}
                  />
                </Form>
                <div style={{ padding: '12px 0', borderTop: '1px solid var(--divider)', textAlign: 'right' }}>
                  <Button
                    color="primary"
                    loading={loading}
                    onClick={this.handleUpdateBoardName}
                  >
                    保存
                  </Button>
                  <Button
                    style={{ marginLeft: 12 }}
                    onClick={() => {
                      this.dataSet.current.set('boardName', lastBoardName);
                    }}
                  >
                    取消
                  </Button>
                </div>
              </div>
            )
          }

        </Spin>

      </div>
    );
  }
}
export default EditBoardName;
