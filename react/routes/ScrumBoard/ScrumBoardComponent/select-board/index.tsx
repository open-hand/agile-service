import React, { useRef } from 'react';
import { Permission } from '@choerodon/boot';
import { Icon } from 'choerodon-ui/pro';
import { observer } from 'mobx-react-lite';
import { FlatSelect } from '@choerodon/components';
import ScrumBoardStore from '@/stores/project/scrumBoard/ScrumBoardStore';
import useSelectFooter from '@/hooks/useSelectFooter';
import styles from './index.less';

const { Option } = FlatSelect;
export interface SelectBoardProps {
  createButton?: boolean
  onChange: (v: string | null) => void
  onFooterClick?: () => void
}
const SelectBoard: React.FC<SelectBoardProps> = ({
  onChange, onFooterClick, createButton = true, ...otherProps
}) => {
  const ref = useRef<FlatSelect>(null);
  const props = useSelectFooter(ref,
    createButton ? (
      <Permission
        service={['choerodon.code.project.cooperation.iteration-plan.ps.board.create']}
      >
        <div role="none" onClick={onFooterClick} className={styles.footer}>
          创建看板
        </div>
      </Permission>
    ) : null);
  return (
    <FlatSelect
      ref={ref}
      value={ScrumBoardStore.getSelectedBoard}
      style={{
        marginLeft: 20, fontWeight: 500,
      }}
      onChange={onChange}
      clearButton={false}
      {...props}
      {...otherProps}
    >

      {
        [...ScrumBoardStore.getBoardList.values()].map((item) => (
          <Option key={item.boardId} value={item.boardId}>
            {item.name}
          </Option>
        ))
      }
    </FlatSelect>
  );
};
export default observer(SelectBoard);
