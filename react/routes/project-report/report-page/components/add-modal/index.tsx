/* eslint-disable no-nested-ternary */
import React, { useEffect, useRef } from 'react';
import { Modal } from 'choerodon-ui/pro';
import { usePersistFn } from 'ahooks';
import { IModalProps, IReportContentType } from '@/common/types';
import MODAL_WIDTH from '@/constants/MODAL_WIDTH';
import AddChart from '../add-chart';
import AddText from '../add-text';
import AddIssueList from '../add-issue-list';
import AddDynamicIssueList from '../add-dynamic-list';
import ProjectReportStore, { IReportBlock } from '../../store';
import to from '@/utils/to';

interface Props {
  modal?: IModalProps,
  type?: IReportContentType
  store: ProjectReportStore
  data?: IReportBlock
  index?: number
}
export interface RefProps {
  /** 图表数据提交，返回 `false` 表示此次图表数据非法 */
  submit: () => Promise<any |false>
}
const Components = new Map<IReportContentType, React.FC<{
  innerRef: React.MutableRefObject<RefProps>
  data?: IReportBlock
  linkTo?:(url: string) => void,
    }>>([
      ['chart', AddChart],
      ['text', AddText],
      ['static_list', AddIssueList],
      ['dynamic_list', AddDynamicIssueList],
    ]);
const AddModal: React.FC<Props> = (props) => {
  const {
    modal, type, store, data: editData, index,
  } = props;
  const isEdit = editData !== undefined;
  const ref = useRef<RefProps>({} as RefProps);
  async function handleSubmit() {
    const data = await ref.current.submit();
    if (data) {
      if (isEdit) {
        store.updateBlock(index as number, data as IReportBlock);
      } else {
        store.addBlock(data as IReportBlock);
      }
      return true;
    }
    return false;
  }
  useEffect(() => {
    modal?.handleOk(handleSubmit);
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, []);
  const linkTo = usePersistFn((linkUrl: string) => {
    modal?.close();
    to(linkUrl);
  });
  const Component = Components.get((editData?.type || type) as IReportContentType);
  if (Component) {
    return (
      <div><Component innerRef={ref} data={editData} linkTo={linkTo} /></div>
    );
  }
  return null;
};
const TEXTS: {
  [key: string]: string
} = {
  chart: '图表',
  text: '文本',
  static_list: '静态列表',
  dynamic_list: '动态列表',
};
const openAddModal = (props: Props) => {
  const { data: editData } = props;
  const isEdit = editData !== undefined;
  const type = props.type || props.data?.type;
  Modal.open({
    key: 'modal',
    title: isEdit ? `修改${TEXTS[type as string] || ''}` : `添加${TEXTS[type as string] || ''}`,
    style: {
      width: type === 'text' || type === 'dynamic_list' ? MODAL_WIDTH.middle : MODAL_WIDTH.large,
    },
    drawer: true,
    children: <AddModal {...props} />,
    okText: isEdit ? '保存' : '确定',
  });
};
export default openAddModal;
