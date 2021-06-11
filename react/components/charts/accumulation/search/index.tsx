import React from 'react';
import { DatePicker, Form } from 'choerodon-ui/pro';
import { Moment } from 'moment';
import { LabelLayout } from 'choerodon-ui/pro/lib/form/enum';
import SelectBoard from './SelectBoard';
import QuickFilter from './QuickFilter';

export interface AccumulationSearchProps {
  range: [Moment, Moment]
  onRangeChange: (value: [Moment, Moment]) => void
  onBoardChange: (boardId: string) => void
  boardId: string
  quickFilterIds: string[]
  onQuickSearchChange: (quickFilterIds: string[]) => void
  projectId?: string
  float?:boolean
}
const AccumulationSearch: React.FC<AccumulationSearchProps> = ({
  range,
  boardId,
  onRangeChange,
  onBoardChange,
  quickFilterIds,
  onQuickSearchChange,
  projectId,
  float,
}) => (
  <Form labelLayout={float ? 'float' as any : 'none'}>
    <div style={{ display: 'flex' }}>
      <div style={{ width: 510, marginLeft: 5 }}>
        <DatePicker
          style={{ width: '100%' }}
          range
          value={range}
          onChange={onRangeChange}
          clearButton={false}
          labelLayout={'float' as LabelLayout}
          label="范围"
        />
      </div>
      <SelectBoard
        projectId={projectId}
        onChange={onBoardChange}
        value={boardId}
      />
      <QuickFilter
        projectId={projectId}
        onChange={onQuickSearchChange}
        value={quickFilterIds}
      />
    </div>
  </Form>
);
export default AccumulationSearch;
