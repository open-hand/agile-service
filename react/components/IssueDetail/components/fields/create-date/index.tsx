import React from 'react';
import { observer } from 'mobx-react-lite';
import { useDetailContext } from '@/components/IssueDetail/context';
import Field from '../field';

const CreateDate: React.FC = () => {
  const { store } = useDetailContext();
  const { issue: { creationDate } } = store;
  return (
    <Field label="创建时间">
      <div style={{ padding: '0 0.05rem 0 0.05rem' }}>
        {creationDate || '-'}
      </div>

    </Field>
  );
};

export default observer(CreateDate);
