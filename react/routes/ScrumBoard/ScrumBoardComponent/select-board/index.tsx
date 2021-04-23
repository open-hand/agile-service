import React, { useRef } from 'react';
import { Permission } from '@choerodon/boot';
import { Button } from 'choerodon-ui/pro';
import { observer } from 'mobx-react-lite';
import FlatSelect from '@/components/flat-select';
import ScrumBoardStore from '@/stores/project/scrumBoard/ScrumBoardStore';
import useSelectFooter from '@/hooks/useSelectFooter';

const { Option } = FlatSelect;
export interface SelectBoardProps {

}
const SelectBoard: React.FC<any> = ({ onChange, onClick }) => {
  const ref = useRef<FlatSelect>(null);
  const props = useSelectFooter(ref,
    <Permission
      service={['choerodon.code.project.cooperation.iteration-plan.ps.board.create']}
    >
      <Button style={{ width: '100%', height: 42, textAlign: 'left' }} onClick={onClick}>创建看板</Button>
    </Permission>);
  return (
    <FlatSelect
      ref={ref}
      value={ScrumBoardStore.getSelectedBoard}
      style={{
        marginRight: 15, fontWeight: 500,
      }}
      onChange={onChange}
      clearButton={false}
      {...props}
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
