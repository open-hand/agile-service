import React, {
  useState, useEffect, useCallback, useRef,
} from 'react';
import { Select } from 'choerodon-ui/pro';
import { find } from 'lodash';
import { IBoard } from '@/common/types';
import { boardApi } from '@/api';

const { Option } = Select;
interface Props {
  onChange: (boardId: string) => void
  value: string
  // eslint-disable-next-line react/require-default-props
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
    <Select
      placeholder="切换看板"
      label="切换看板"
      value={value}
      clearButton={false}
      style={{ marginLeft: 16 }}
      onChange={(boardId) => {
        onChange(boardId);
      }}
    >
      {boardList.map((board) => (
        <Option
          value={board.boardId}
        >
          {board.name}
        </Option>
      ))}
    </Select>
  );
};
export default SelectBoard;
