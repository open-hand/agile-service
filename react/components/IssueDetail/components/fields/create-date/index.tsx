import React from 'react';
import { observer } from 'mobx-react-lite';
import { useDetailContext } from '@/components/IssueDetail/context';
import DatetimeAgo from '@/components/CommonComponent/DatetimeAgo';
import Field from '../field';

const CreateDate: React.FC = () => {
  const { store } = useDetailContext();
  const { issue: { creationDate } } = store;
  return (
    <Field label="创建时间">
      <div style={{ padding: '0 0.05rem 0 0.05rem' }}>
        {creationDate ? (

          <DatetimeAgo
            date={creationDate}
          />
        ) : '-'}
      </div>

    </Field>
  );
};

export default observer(CreateDate);
