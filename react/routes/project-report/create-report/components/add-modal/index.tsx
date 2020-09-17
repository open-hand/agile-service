import React, { useEffect, useRef } from 'react';
import { Modal } from 'choerodon-ui/pro';
import { IModalProps, IReportContentType } from '@/common/types';
import AddChart from '../add-chart';
import AddText from '../add-text';
import AddIssueList from '../add-issue-list';
import ProjectReportStore, { IReportBlock } from '../../store';

interface Props {
  modal?: IModalProps,
  type: IReportContentType
  store: ProjectReportStore
}
export interface RefProps {
  submit: () => Promise<any>
}
const Components = new Map<IReportContentType, React.FC<{ innerRef: React.MutableRefObject<RefProps> }>>([
  ['chart', AddChart],
  ['text', AddText],
  ['list', AddIssueList],
  ['static_list', AddIssueList],
]);
const AddModal: React.FC<Props> = ({
  modal, type, store,
}) => {
  const ref = useRef<RefProps>({} as RefProps);
  async function handleSubmit() {
    const data = await ref.current.submit();
    console.log(data);
    if (data) {
      store.addBlock(data as IReportBlock);
      return true;
    }
    return false;
  }
  useEffect(() => {
    modal?.handleOk(handleSubmit);
  // eslint-disable-next-line react-hooks/exhaustive-deps
  }, []);
  const Component = Components.get(type);
  if (Component) {
    return (
      <div><Component innerRef={ref} /></div>
    );
  }
  return null;
};

const openAddModal = (props: Props) => {
  Modal.open({
    key: Modal.key(),
    title: '添加',
    style: {
      width: props.type === 'text' ? 380 : 1088,
    },
    drawer: true,
    children: <AddModal {...props} />,
  });
};
export default openAddModal;
