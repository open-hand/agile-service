// import React, {
//   useEffect, useRef, useCallback, useMemo,
// } from 'react';
// import { Modal } from 'choerodon-ui/pro';
// import { IModalProps } from '@/common/types';
// import MODAL_WIDTH from '@/constants/MODAL_WIDTH';
// import Filter, { useFilter } from '@/components/filter';
// import { filterToSearchVO, ISearchVO, SearchVOToFilter } from './utils';
// import { FilterConfig } from './useFilter';

// interface Props extends FilterConfig {
//   modal?: IModalProps
//   searchVO?: ISearchVO
//   onOK: (searchVO: ISearchVO) => void
// }
// export interface RefProps {
//   validate: () => Promise<boolean>
// }

// const FilterModal: React.FC<Props> = ({
//   modal, searchVO = {}, onOK, ...configProps
// }) => {
//   const ref = useRef<RefProps>({} as RefProps);
//   const initFilter = SearchVOToFilter(searchVO);
//   const config: FilterConfig = useMemo(() => ({
//     filter: initFilter,
//     selected: Object.keys(initFilter).filter((key) => initFilter[key] !== undefined),
//     ...configProps,
//   }), [configProps, initFilter]);
//   const { state, handleSelectChange, handleFilterChange } = useFilter(config);
//   const handleSubmit = useCallback(async () => {
//     const validate = await ref.current.validate();
//     if (validate) {
//       const searchVOResult = filterToSearchVO(state.filter, [...state.systemFields, ...state.customFields]);
//       onOK(searchVOResult);
//       return true;
//     }
//     return false;
//   }, [onOK, state.customFields, state.filter, state.systemFields]);
//   useEffect(() => {
//     modal?.handleOk(handleSubmit);
//   }, [handleSubmit, modal]);

//   return (
//     <>
//       <Filter
//         systemFields={state.systemFields}
//         customFields={state.customFields}
//         innerRef={ref}
//         value={state.filter}
//         selected={state.selected}
//         onSelectChange={handleSelectChange}
//         onFilterChange={handleFilterChange}
//       />
//     </>
//   );
// };

// const openFilterModal = (props: Props) => {
//   Modal.open({
//     key: 'modal',
//     title: '筛选',
//     style: {
//       width: MODAL_WIDTH.small,
//     },
//     drawer: true,
//     children: <FilterModal {...props} />,
//   });
// };
// export default openFilterModal;
