import React, { useState, useEffect, useCallback } from 'react';
import {
  CheckBox, Button, Icon,
} from 'choerodon-ui/pro';
import { find } from 'lodash';
import { Popover } from 'choerodon-ui';
import { ButtonColor } from 'choerodon-ui/pro/lib/button/enum';
import { IBoard } from '@/common/types';
import { boardApi } from '@/api';

interface Props {
  onChange: (boardId: string) => void
  value: string
}
const SelectBoard: React.FC<Props> = ({
  onChange, value,
}) => {
  const [boardList, setBoardList] = useState<IBoard[]>([]);
  const refresh = useCallback(async () => {
    const list = await boardApi.loadAll();
    const defaultBoard = find(list, { userDefault: true });
    if (list.length > 0) {
      onChange(defaultBoard ? defaultBoard.boardId : list[0].boardId);
    }
    setBoardList(list);
  }, []);
  useEffect(() => {
    refresh();
  }, [refresh]);

  return (
    <Popover
      placement="bottom"
      trigger="click"
      content={boardList.map((board) => (
        <CheckBox
          key={board.boardId}
          style={{ display: 'block', margin: '5px 0' }}
          checked={board.boardId === value}
          onChange={(checked) => {
            if (board.boardId !== value) {
              onChange(board.boardId);
            }
          }}
        >
          {board.name}
        </CheckBox>
      ))}
    >
      <Button
        color={'blue' as ButtonColor}
        style={{
          marginLeft: 20,
        }}
      >
        看板
        <Icon type="baseline-arrow_drop_down" style={{ marginTop: -3 }} />
      </Button>
    </Popover>
  );
};
export default SelectBoard;
