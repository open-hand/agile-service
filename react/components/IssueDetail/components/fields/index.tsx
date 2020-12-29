import React from 'react';
import { observer } from 'mobx-react-lite';
import { useDetailContext } from '../../context';
import Section from '../section';
import CustomFields from './custom-fields';

const Fields: React.FC = () => {
  const {
    store, outside, disabledDetailEdit,
  } = useDetailContext();
  const statusCode = store.issue?.statusVO?.code;
  const readonly = disabledDetailEdit;
  return (
    <Section title="详情" style={{ marginTop: 0 }} border>
      <CustomFields />
    </Section>
  );
};

export default observer(Fields);
