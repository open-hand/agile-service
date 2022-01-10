import React from 'react';
import { observer } from 'mobx-react-lite';
import Header from './Header';
import Content from './Content';
import BatchMove from './BatchMove';
import Modal from '../Modal';

function BatchModal(props) {
  const { issueSearchStore } = props;
  let ContentComponent = React.Fragment;
  switch (issueSearchStore.batchAction) {
    case 'edit': {
      ContentComponent = Content;
      break; }
    case 'move': {
      ContentComponent = BatchMove;
      break;
    }
    default:
      break;
  }
  return (
    <Modal header={<Header {...props} />} content={<ContentComponent {...props} />} />
  );
}
export default observer(BatchModal);
