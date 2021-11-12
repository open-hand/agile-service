import { DatePickerProps } from 'choerodon-ui/pro/lib/date-picker/DatePicker';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import { IField } from '@/common/types';

interface IChosenFieldField extends Omit<Partial<IField>, 'fieldType'> {
    id?: string,
    code: string,
    name: string,
    defaultShow?: boolean,
    fieldType?: string,
    noDisplay?: boolean,
    /** 是否为归档字段 */
    archive?: boolean,
    immutableCheck?: boolean, // 存在此属性 则不可删除 禁止选择
    otherComponentProps?: Partial<SelectProps> | Partial<DatePickerProps> // 组件的属性
    value?: any,

}
interface IUseChoseFieldProps {
    systemFields: IChosenFieldField[]
    customFields: IChosenFieldField[],
    chosenFields?: IChosenFieldField[],
    addFieldCallback?: (key: string) => void
}
/**
 * @param initField 初始化fields字段时的操作 返回false | undefined | void 则跳过此字段
 * @param initChosenField 初始化已选字段时的操作 返回false | undefined | void 则跳过此字段
 */
interface IChosenFieldFieldEvents {
    /** 开始初始化字段 */
    initFieldStart?: (fields: IChosenFieldField[], currentChosenField: Map<string, IChosenFieldField>) => void,
    /** 开始初始化单个字段 */
    initField?: (data: IChosenFieldField, currentChosenField: Map<string, IChosenFieldField>) => IChosenFieldField | false | undefined | void,
    /** 初始化字段字段完成 */
    initFieldFinish?: (customFields: IChosenFieldField[], systemFields: IChosenFieldField[], currentChosenField: Map<string, IChosenFieldField>) => void,
    /** 初始化传入的默认值中每个已选字段 */
    initChosenField?: (data: IChosenFieldField, currentChosenField: Map<string, IChosenFieldField>) => IChosenFieldField | false | undefined | void,
    /** 选择字段事件 */
    choseField?: (data: IChosenFieldField | IChosenFieldField[], status: 'add' | 'del') => void,
    /** 取消选择字段事件 */
    cancelChosenField?: (data: IChosenFieldField) => void,
}
export { IChosenFieldField, IUseChoseFieldProps, IChosenFieldFieldEvents };
