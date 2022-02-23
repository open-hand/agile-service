import React, { useRef } from 'react';
import { C7NFormat } from '@choerodon/master';
import { Permission } from '@choerodon/boot';
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
        service={[
          'choerodon.code.project.cooperation.iteration-plan.ps.board.create',
          'choerodon.code.project.cooperation.sprint.iteration-plan.ps.board.create',
        ]}
      >
        <div role="none" onClick={onFooterClick} className={styles.footer}>
          <C7NFormat
            intlPrefix="agile.scrumBoard"
            id="create.board"
          />
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
      className={styles.boardSelect}
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
