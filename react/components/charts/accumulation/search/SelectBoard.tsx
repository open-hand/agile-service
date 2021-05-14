import React, {
  useState, useEffect, useCallback, useRef,
} from 'react';
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
  projectId?: string
}
const SelectBoard: React.FC<Props> = ({
  onChange, value, projectId,
}) => {
  const [boardList, setBoardList] = useState<IBoard[]>([]);
  const valueRef = useRef(value);
  valueRef.current = value;
  const refresh = useCallback(async () => {
    const list = await boardApi.project(projectId).loadAll();
    const defaultBoard = find(list, { userDefault: true });
    if (!valueRef.current && list.length > 0) {
      onChange(defaultBoard ? defaultBoard.boardId : list[0].boardId);
    }
    setBoardList(list);
  }, [onChange, projectId]);
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
        color={'primary' as ButtonColor}
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
