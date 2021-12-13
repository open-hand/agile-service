import React from 'react';
import { observer } from 'mobx-react-lite';
import Header from './Header';
import Content from './Content';
import BatchMove from './BatchMove';
import Modal from '../Modal';

function BatchModal(props) {
  const { issueSearchStore } = props;
  const ContentComponent = issueSearchStore.batchAction === 'edit' ? Content : BatchMove;
  return (
    <Modal header={<Header {...props} />} content={<ContentComponent {...props} />} />
  );
}
export default observer(BatchModal);
