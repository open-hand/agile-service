import React, {useEffect} from 'react';
import {observer} from 'mobx-react-lite';
import {Button, Icon} from 'choerodon-ui/pro';
import {AssigneeModal, openAssigneeModal} from './AssigneeModal';
import './WorkLoadBtn.less';
import useFormatMessage from '@/hooks/useFormatMessage';
import BacklogStore from "@/stores/project/backlog/BacklogStore";
import MODAL_WIDTH from "@/constants/MODAL_WIDTH";

let modalRef = null;

const WorkLoadBtn = ({ data, refresh }) => {

  /**
   * 监听工作列表主页的数据变更，刷新工作量弹窗
   */
  useEffect(() => {
    if(!modalRef) {
      return;
    }
    modalRef.update(openModalProps({
      data,
      refresh,
      loading: false
    }));
  }, [JSON.stringify(data)])

  /**
   * 调用工作列表主页的store的刷新方法加载新数据
   */
  const handelRefresh = () => {
    // 为什么这里要update, 因为Modal组件的children组件的props, Modal不update不会更新, 无法实现loading效果
    modalRef.update(openModalProps({
      data,
      refresh,
      loading: true
    }));
    refresh()
  }

  const openModalProps = (props) => {
    return {
      title:(
        <div style={{ display: 'flex', justifyContent: 'space-between', alignItems: 'center' }}>
          <span>经办人工作量</span>
          <Button icon="refresh" color="primary" onClick={handelRefresh} />
        </div>),
      style: {
        width: MODAL_WIDTH.large,
      },
      drawer: true,
      okText: '关闭',
      footer: (ok) => ok,
      children: <AssigneeModal {...props} />,
      onOk: () => { BacklogStore.setModalOpened(false); return true; },
    }
  }

  const handleClickBtn = () => {
    modalRef = openAssigneeModal(openModalProps({
      data,
      refresh,
      loading: false
    }));
  };
  const { assigneeIssues } = data;
  const formatMessage = useFormatMessage('agile.backlog');
  return (
    <>
      <span
        role="none"
        onClick={handleClickBtn}
        className="c7n-agile-workloadBtn"
        style={{
          display: assigneeIssues && assigneeIssues.length > 0 ? 'flex' : 'none',
        }}
      >
        <Icon type="find_in_page-o" />
        <span>{formatMessage({ id: 'assignee.workload' })}</span>
      </span>
    </>
  );
};

export default observer(WorkLoadBtn);
