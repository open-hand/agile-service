import React from 'react';
import { observer } from 'mobx-react-lite';
import IssueTypeTag from '@/components/TypeTag';
import styles from './index.less';
import { useDetailContext } from '../../context';

const TypeTag: React.FC = () => {
  const { store } = useDetailContext();
  const { issueTypeVO = {}, featureVO } = store.issue;
  return (
    <IssueTypeTag
        // @ts-ignore
      data={issueTypeVO}
      featureType={featureVO?.featureType}
    />
  );
};

export default observer(TypeTag);
