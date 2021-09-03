import React from 'react';
import { DatePicker, Form } from 'choerodon-ui/pro';
import { Moment } from 'moment';
import { LabelLayout } from 'choerodon-ui/pro/lib/form/enum';
import SelectBoard from './SelectBoard';
import SelectQuickFilterField from '@/components/select/select-quick-filter';

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
      <div style={{ width: 300 }}>
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
      <SelectQuickFilterField
        projectId={projectId}
        onChange={onQuickSearchChange}
        commonOptions={false}
        multiple
        placeholder="快速筛选"
        label="快速筛选"
        optionFlat
        style={{ marginLeft: 16 }}
        value={quickFilterIds}
      />
    </div>
  </Form>
);
export default AccumulationSearch;
