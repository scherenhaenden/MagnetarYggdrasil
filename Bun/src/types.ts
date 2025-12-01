export interface User {
    id?: number;
    name: string;
    email: string;
}

export interface Task {
    id?: number;
    user_id: number;
    title: string;
    description: string;
    is_done: boolean;
}
