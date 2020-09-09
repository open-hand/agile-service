import React from 'react';
import { DatePicker } from 'choerodon-ui/pro';
import { Moment } from 'moment';
import SelectBoard from './SelectBoard';
import QuickFilter from './QuickFilter';

export interface AccumulationSearchProps {
  range: [Moment, Moment]
  onRangeChange: (value: [Moment, Moment]) => void
  onBoardChange: (boardId: string, checked: boolean) => void
  boardId: string
  quickFilterIds: string[]
  onQuickSearchChange: (quickFilterIds: string[]) => void
}
const AccumulationSearch: React.FC<AccumulationSearchProps> = ({
  range,
  boardId,
  onRangeChange,
  onBoardChange,
  quickFilterIds,
  onQuickSearchChange,
}) => (
  <div>
    <DatePicker
      range
      value={range}
      onChange={onRangeChange}
      clearButton={false}
    />
    <SelectBoard
      onChange={onBoardChange}
      value={boardId}
    />
    <QuickFilter
      onChange={onQuickSearchChange}
      value={quickFilterIds}
    />
  </div>
);
export default AccumulationSearch;
