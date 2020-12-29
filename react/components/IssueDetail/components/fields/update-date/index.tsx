import React from 'react';
import { observer } from 'mobx-react-lite';
import { useDetailContext } from '@/components/IssueDetail/context';
import DatetimeAgo from '@/components/CommonComponent/DatetimeAgo';
import Field from '../field';

const CreateDate: React.FC = () => {
  const { store } = useDetailContext();
  const { issue: { lastUpdateDate } } = store;
  return (
    <Field label="更新时间">
      <div style={{ padding: '0 0.05rem 0 0.05rem' }}>
        {lastUpdateDate ? (
          <DatetimeAgo
            date={lastUpdateDate}
          />
        ) : '-'}
      </div>
    </Field>
  );
};

export default observer(CreateDate);
