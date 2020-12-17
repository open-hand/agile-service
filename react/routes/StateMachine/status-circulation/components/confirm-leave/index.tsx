import React from 'react';
import { omit } from 'lodash';
import { Modal } from 'choerodon-ui/pro';

const key = Modal.key();
interface Props {
  modal?: any
}
const ConfirmLeave: React.FC<Props> = ({
  modal,
}) => (
  <div style={{ marginTop: '-.24rem' }}>当前页存在未保存内容，确定离开？</div>
);
const openConfirmLeave = (props: {
  onOk: Function
}) => {
  Modal.open({
    title: '有未保存更改',
    key,
    onOk: props.onOk,
    children: <ConfirmLeave {...omit(props, 'onOk')} />,
  });
};
export default openConfirmLeave;
