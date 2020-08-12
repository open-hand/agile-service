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
  <div>提交并退出？</div>
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
