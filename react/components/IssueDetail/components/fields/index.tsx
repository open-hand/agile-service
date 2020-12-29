import React from 'react';
import { observer } from 'mobx-react-lite';
import { useDetailContext } from '../../context';
import Section from '../section';
import Status from './status';
import Priority from './priority';
import CreateDate from './create-date';
import UpdateDate from './update-date';
import styles from './index.less';
import CustomFields from './custom-fields';

const Fields: React.FC = () => {
  const {
    store, outside, disabledDetailEdit,
  } = useDetailContext();
  const statusCode = store.issue?.statusVO?.code;
  const readonly = disabledDetailEdit;
  return (
    <Section title="详情" style={{ marginTop: 0 }} border>
      <div className={styles.fields}>
        <Status readonly />
        <Priority readonly={readonly} />
        <CreateDate />
        {
          (!outside && (statusCode !== 'backlog_pending_approval' && statusCode !== 'backlog_rejected')) && <UpdateDate />
        }
        <CustomFields />
      </div>
    </Section>
  );
};

export default observer(Fields);
