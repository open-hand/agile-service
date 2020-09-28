import { IField } from '@/common/types';
import { DatePickerProps } from 'choerodon-ui/pro/lib/date-picker/DatePicker';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';

interface IChosenFieldField extends Partial<IField> {
    id?: string,
    code: string,
    name: string,
    defaultShow?: boolean,
    fieldType?: string,
    noDisplay?: boolean,
    immutableCheck?: boolean, // 存在此属性 则不可删除 禁止选择
    otherComponentProps?: Partial<SelectProps> | Partial<DatePickerProps> // 组件的属性
    value?: any,

}
interface IUseChoseFieldProps {
    systemFields: IChosenFieldField[]
    customFields: IChosenFieldField[],
    chosenFields?: IChosenFieldField[],
}
/**
 * @param initField 初始化fields字段时的操作 返回false | undefined | void 则跳过此字段
 * @param initChosenField 初始化已选字段时的操作 返回false | undefined | void 则跳过此字段
 */
interface IChosenFieldFieldEvents {
    initField?: (data: IChosenFieldField, currentChosenField: Map<string, IChosenFieldField>) => IChosenFieldField | false | undefined | void,
    initChosenField?: (data: IChosenFieldField, currentChosenField: Map<string, IChosenFieldField>) => IChosenFieldField | false | undefined | void,
}
export { IChosenFieldField, IUseChoseFieldProps, IChosenFieldFieldEvents };
