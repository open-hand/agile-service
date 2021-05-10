import React from 'react';
import Header from './Header';
import Content from './Content';
import Modal from '../Modal';

function BatchModal(props) {
  return (
    <>
      <Modal header={<Header {...props} />} content={<Content {...props} />} />
    </>
  );
}
export default BatchModal;
