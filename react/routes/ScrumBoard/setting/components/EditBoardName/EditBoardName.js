import React, {
  useCallback, useEffect, useMemo, useState,
} from 'react';
import {
  Choerodon,
} from '@choerodon/boot';
import {
  Form, TextField, Button, Spin, DataSet,
} from 'choerodon-ui/pro';
import { observer } from 'mobx-react-lite';
import ScrumBoardStore from '@/stores/project/scrumBoard/ScrumBoardStore';
import { boardApi } from '@/api';

const EditBoardName = ({ editBoardNameDisabled }) => {
  const [loading, setLoading] = useState(false);

  useEffect(() => {
    const currentEditBoard = ScrumBoardStore.getBoardList.get(ScrumBoardStore.getSelectedBoard);
    const { name } = currentEditBoard || {};
    dataSet.loadData([{ boardName: name }]);
  }, [ScrumBoardStore.getSelectedBoard]);

  const checkBoardNameRepeat = useCallback(async (value, name, record) => {
    const initialBoardName = record.getPristineValue(name);
    if (initialBoardName === value) {
      return true;
    }
    try {
      const res = await boardApi.checkName(value);
      if (res) {
        return '看板名称重复';
      }
      return true;
    } catch (e) {
      return false;
    }
  }, []);

  const dataSet = useMemo(() => new DataSet({
    autoCreate: false,
    fields: [{
      name: 'boardName',
      required: true,
      validator: checkBoardNameRepeat,
    }],
  }), []);

  const handleUpdateBoardName = useCallback(async () => {
    const currentEditBoard = ScrumBoardStore.getBoardList.get(ScrumBoardStore.getSelectedBoard);
    const { objectVersionNumber, boardId, projectId } = currentEditBoard;
    try {
      if (await dataSet.validate()) {
        const boardName = dataSet.current.get('boardName');
        const data = {
          objectVersionNumber,
          boardId,
          name: boardName,
          projectId,
        };
        setLoading(true);
        const res = await boardApi.update(data.boardId, data);
        if (res) {
          ScrumBoardStore.setBoardList(ScrumBoardStore.getSelectedBoard, res);
          Choerodon.prompt('保存成功');
          dataSet.current?.init('boardName', boardName);
        }
        setLoading(false);
      } else {
        setLoading(false);
      }
    } catch (e) {
      setLoading(false);
      dataSet.reset();
    }
  }, [dataSet, ScrumBoardStore.getSelectedBoard]);

  return (
    <div style={{ marginLeft: 5 }}>
      <Spin spinning={loading}>
        {
          editBoardNameDisabled ? (
            <TextField
              style={{ width: 512, marginTop: 5 }}
              label="看板名称"
              maxLength={10}
              valueChangeAction="input"
              value={dataSet?.current?.getPristineValue('boardName')}
              disabled
            />
          ) : (
            <div>
              <Form dataSet={dataSet} style={{ width: 512 }}>
                <TextField
                  name="boardName"
                  label="看板名称"
                  maxLength={10}
                />
              </Form>
              <div style={{ padding: '12px 0', borderTop: '1px solid var(--divider)' }}>
                <Button
                  color="primary"
                  loading={loading}
                  onClick={handleUpdateBoardName}
                >
                  保存
                </Button>
                <Button
                  style={{ marginLeft: 12 }}
                  onClick={() => {
                    dataSet.reset();
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
};

export default observer(EditBoardName);
